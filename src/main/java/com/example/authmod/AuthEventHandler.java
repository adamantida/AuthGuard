package com.example.authmod;

import cpw.mods.fml.common.eventhandler.SubscribeEvent;
import cpw.mods.fml.common.gameevent.PlayerEvent;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.network.play.server.S08PacketPlayerPosLook;
import net.minecraft.util.ChatComponentText;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.item.ItemTossEvent;
import net.minecraftforge.event.entity.living.LivingEvent;
import net.minecraftforge.event.entity.player.*;
import java.lang.reflect.Field;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Обработчик событий аутентификации.
 * Контролирует состояние авторизации игроков и ограничивает действия
 * неавторизованных пользователей.
 */
public class AuthEventHandler {

    /** Максимальное время на авторизацию (3 минуты) */
    public static final long MAX_LOGIN_TIME = 3 * 60 * 1000;

    /** Интервал проверки (раз в 5 тиков = 0.25 секунды) */
    private static final int CHECK_INTERVAL = 5;

    /** Допуск позиции для уменьшения частоты телепортации */
    private static final double POSITION_TOLERANCE = 0.75;

    /** Допуск для проверки позиции */
    private static final double POSITION_CHECK_TOLERANCE = 0.25;

    /** Интервал для обновления времени активности */
    private static final int ACTIVITY_UPDATE_INTERVAL = 20;

    // ========================================================================
    // Хранилище данных игроков
    // ========================================================================

    /** Последние валидные позиции игроков */
    private static final Map<String, double[]> LAST_VALID_POSITION = new ConcurrentHashMap<>();

    /** Счетчик тиков после входа */
    private static final Map<String, Integer> LOGIN_TICK_COUNTER = new ConcurrentHashMap<>();

    /** Флаг инициализации позиции */
    private static final Map<String, Boolean> POSITION_INITIALIZED = new ConcurrentHashMap<>();

    /** Флаг аутентификации игроков */
    private static final Map<String, Boolean> AUTHENTICATED_PLAYERS = new ConcurrentHashMap<>();

    /** Время последней активности */
    private static final Map<String, Long> LOGIN_TIME_MAP = new ConcurrentHashMap<>();

    /** Пул потоков для отправки сообщений */
    private static final ExecutorService LOGIN_MESSAGE_EXECUTOR =
            Executors.newFixedThreadPool(2, r -> {
                Thread t = new Thread(r);
                t.setName("Auth-Login-Messages");
                t.setDaemon(true);
                return t;
            });

    // ========================================================================
    // Публичные методы
    // ========================================================================

    /**
     * Нормализует имя пользователя (приводит к нижнему регистру)
     */
    public static String normalizeUsername(String username) {
        return (username != null) ? username.toLowerCase() : "unknown";
    }

    /**
     * Проверяет, авторизован ли игрок
     */
    public static boolean isPlayerAuthenticated(EntityPlayer player) {
        if (player == null) return false;

        String username = normalizeUsername(player.getCommandSenderName());
        return Boolean.TRUE.equals(AUTHENTICATED_PLAYERS.get(username));
    }

    /**
     * Аутентифицирует игрока
     */
    public static void authenticatePlayer(EntityPlayer player) {
        if (player == null) return;

        String username = normalizeUsername(player.getCommandSenderName());

        // Обновление состояния
        AUTHENTICATED_PLAYERS.put(username, true);
        LOGIN_TIME_MAP.remove(username);
        LAST_VALID_POSITION.remove(username);
        POSITION_INITIALIZED.remove(username);

        // Обновляем IP в данных
        if (player instanceof EntityPlayerMP) {
            String ip = getPlayerIP((EntityPlayerMP) player);
            PlayerDataManager.updateLoginData(username, ip);
        }

        // Уведомление игрока
        sendPrivateMessage(player, "§aАвторизация успешна!");
    }

    /**
     * Обновляет время последней активности игрока
     */
    public static void updateLoginTime(String username) {
        String normalizedUsername = normalizeUsername(username);
        if (LOGIN_TIME_MAP.containsKey(normalizedUsername)) {
            LOGIN_TIME_MAP.put(normalizedUsername, System.currentTimeMillis());
        }
    }

    /**
     * Возвращает IP-адрес игрока
     */
    public static String getPlayerIP(EntityPlayerMP player) {
        try {
            String ip = player.getPlayerIP();
            // Удаляем порт, если он есть
            if (ip.contains(":")) {
                return ip.substring(0, ip.indexOf(':'));
            }
            return ip;
        } catch (Exception e) {
            return "unknown";
        }
    }

    /**
     * Завершает работу пула потоков при выключении сервера
     */
    public static void shutdown() {
        LOGIN_MESSAGE_EXECUTOR.shutdown();
    }

    // ========================================================================
    // Обработчики событий
    // ========================================================================

    @SubscribeEvent
    public void onPlayerLogin(PlayerEvent.PlayerLoggedInEvent event) {
        handlePlayerLogin(event.player);
    }

    @SubscribeEvent
    public void onPlayerLogout(PlayerEvent.PlayerLoggedOutEvent event) {
        clearPlayerData(normalizeUsername(event.player.getCommandSenderName()));
    }

    @SubscribeEvent
    public void onPlayerMove(LivingEvent.LivingUpdateEvent event) {
        if (event.entityLiving instanceof EntityPlayer) {
            handlePlayerMovement((EntityPlayer) event.entityLiving);
        }
    }

    @SubscribeEvent
    public void onPlayerInteract(PlayerInteractEvent event) {
        if (event.entityPlayer != null) {
            handlePlayerInteraction(event);
        }
    }

    @SubscribeEvent
    public void onPlayerAttack(AttackEntityEvent event) {
        if (event.entityPlayer != null) {
            handlePlayerAttack(event);
        }
    }

    @SubscribeEvent
    public void onItemPickup(EntityItemPickupEvent event) {
        if (event.entityPlayer != null) {
            handleItemPickup(event);
        }
    }

    @SubscribeEvent
    public void onItemToss(ItemTossEvent event) {
        if (event.player != null) {
            handleItemToss(event);
        }
    }

    // ========================================================================
    // Внутренние методы обработки
    // ========================================================================

    private void handlePlayerLogin(EntityPlayer player) {
        String username = normalizeUsername(player.getCommandSenderName());

        // Инициализация данных
        AUTHENTICATED_PLAYERS.put(username, false);
        LOGIN_TIME_MAP.put(username, System.currentTimeMillis());
        LOGIN_TICK_COUNTER.put(username, 0);
        POSITION_INITIALIZED.put(username, false);

        // Немедленно инициализируем позицию
        initializePlayerPosition(player, username);
        POSITION_INITIALIZED.put(username, true);

        // Отправка сообщения с задержкой
        scheduleLoginMessage(player, username);

        AuthMod.logger.info("Player login initialized: " + username);
    }

    private void scheduleLoginMessage(EntityPlayer player, String username) {
        LOGIN_MESSAGE_EXECUTOR.submit(() -> {
            try {
                Thread.sleep(2000);
                sendLoginInstructions(player, username);
            } catch (InterruptedException e) {
                AuthMod.logger.error("Failed to send login message to " + username, e);
            }
        });
    }

    private void sendLoginInstructions(EntityPlayer player, String username) {
        if (player == null || player.worldObj.isRemote) return;

        if (Boolean.FALSE.equals(AUTHENTICATED_PLAYERS.get(username))) {
            String message = PlayerDataManager.isPlayerRegistered(username)
                    ? "§6Введите /auth login <пароль> для авторизации"
                    : "§6Введите /auth register <пароль> <подтверждение> для регистрации";

            player.addChatMessage(new ChatComponentText(message));
            AuthMod.logger.info("Login message sent to player: " + username);
        }
    }

    private void handlePlayerMovement(EntityPlayer player) {
        String username = normalizeUsername(player.getCommandSenderName());

        // Пропускаем некоторые тики для снижения нагрузки
        if (player.ticksExisted % CHECK_INTERVAL != 0) {
            return;
        }

        // Обновляем счетчик тиков после входа
        updateLoginTickCounter(username, player);

        // Инициализируем запись, если её нет
        initializePlayerRecord(player, username);

        // Сброс таймера активности (раз в 20 тиков)
        if (player.ticksExisted % ACTIVITY_UPDATE_INTERVAL == 0) {
            updateLoginTime(username);
        }

        // Обработка неавторизованных игроков
        if (!isPlayerAuthenticated(player)) {
            handleUnauthorizedPlayer(player, username);
        }
    }

    private void updateLoginTickCounter(String username, EntityPlayer player) {
        if (LOGIN_TICK_COUNTER.containsKey(username)) {
            int ticks = LOGIN_TICK_COUNTER.get(username);
            if (ticks < 40) { // Ждем 2 секунды, чтобы игрок приземлился
                LOGIN_TICK_COUNTER.put(username, ticks + 1);
            } else {
                // Инициализируем позицию после 2 секунд
                if (!Boolean.TRUE.equals(POSITION_INITIALIZED.get(username))) {
                    initializePlayerPosition(player, username);
                    POSITION_INITIALIZED.put(username, true);
                    AuthMod.logger.info("Position initialized for " + username +
                            " at (" + player.posX + ", " + player.posY + ", " + player.posZ + ")");
                }
            }
        }
    }

    private void initializePlayerRecord(EntityPlayer player, String username) {
        if (!AUTHENTICATED_PLAYERS.containsKey(username)) {
            AuthMod.logger.warn("Player has no auth record, treating as unauthenticated: " + username);
            AUTHENTICATED_PLAYERS.put(username, false);
            LOGIN_TIME_MAP.put(username, System.currentTimeMillis());
            initializePlayerPosition(player, username);
        }
    }

    private void handleUnauthorizedPlayer(EntityPlayer player, String username) {
        // Блокировка движения
        handleUnauthorizedMovement(player, username);

        // Проверка таймера авто-логаута
        checkLoginTimeout(player, username);

        // Отправка сообщений
        sendAuthMessages(player, username);
    }

    private void checkLoginTimeout(EntityPlayer player, String username) {
        Long loginTime = LOGIN_TIME_MAP.get(username);
        if (loginTime == null) return;

        long currentTime = System.currentTimeMillis();
        long timeSinceLogin = currentTime - loginTime;

        if (timeSinceLogin > MAX_LOGIN_TIME) {
            // Кикаем игрока через 3 минуты неактивности
            if (player instanceof EntityPlayerMP) {
                EntityPlayerMP playerMP = (EntityPlayerMP) player;
                playerMP.playerNetServerHandler.kickPlayerFromServer("Время на авторизацию истекло!");
            }
            return;
        }
    }

    private void sendAuthMessages(EntityPlayer player, String username) {
        Long loginTime = LOGIN_TIME_MAP.get(username);
        if (loginTime == null) return;

        long currentTime = System.currentTimeMillis();
        long timeSinceLogin = currentTime - loginTime;

        // Предупреждение за 1 минуту до логаута
        if (timeSinceLogin > (MAX_LOGIN_TIME - 60000) && player.ticksExisted % 100 == 0) {
            long timeLeft = (MAX_LOGIN_TIME - timeSinceLogin) / 1000;
            sendPrivateMessage(player, "§cАвторизуйтесь в течение " + timeLeft + " секунд!");
        }

        // Отправка основного сообщения раз в 5 секунд
        if (player.ticksExisted % 100 == 0) {
            sendPrivateMessage(player, "Требуется авторизация!");
        }
    }

    // ИСПРАВЛЕННЫЙ МЕТОД: инициализация позиции после входа (решает проблему с погружением в землю)
    public static void initializePlayerPosition(EntityPlayer player, String username) {
        World world = player.worldObj;

        // Находим безопасную позицию над блоком
        int x = (int) Math.floor(player.posX);
        int y = (int) Math.floor(player.posY);
        int z = (int) Math.floor(player.posZ);

        // Поднимаем игрока над блоком
        double safeY = y + 1.0;

        // Проверяем, что позиция безопасна
        if (world.getBlock(x, (int) safeY, z).getMaterial().isSolid()) {
            safeY = y + 2.0; // Если блок занят, поднимаем выше
        }

        double[] position = {player.posX, safeY, player.posZ};
        LAST_VALID_POSITION.put(username, position);

        // Применяем позицию
        player.setPositionAndUpdate(position[0], position[1], position[2]);
    }

    public static void deauthenticatePlayer(EntityPlayer player) {
        if (player == null) return;

        String username = normalizeUsername(player.getCommandSenderName());
        clearPlayerData(username);
        AUTHENTICATED_PLAYERS.put(username, false);
        LOGIN_TIME_MAP.put(username, System.currentTimeMillis());

        // Сброс позиции
        if (player instanceof EntityPlayerMP) {
            initializePlayerPosition(player, username);
        }
    }

    // ОПТИМИЗИРОВАННЫЙ МЕТОД: полная блокировка движения
    private void handleUnauthorizedMovement(EntityPlayer player, String username) {
        // Если позиция еще не инициализирована, инициализируем её немедленно
        if (!LAST_VALID_POSITION.containsKey(username)) {
            initializePlayerPosition(player, username);
        }

        // Получаем последнюю валидную позицию
        double[] lastPos = LAST_VALID_POSITION.get(username);

        // Принудительная телепортация на сервере
        player.setPosition(lastPos[0], lastPos[1], lastPos[2]);

        // Синхронизация с клиентом через пакет (каждые 5 тиков)
        if (player instanceof EntityPlayerMP && player.ticksExisted % 5 == 0) {
            EntityPlayerMP playerMP = (EntityPlayerMP) player;
            playerMP.playerNetServerHandler.sendPacket(
                    new S08PacketPlayerPosLook(lastPos[0], lastPos[1], lastPos[2],
                            player.rotationYaw, player.rotationPitch, false)
            );
        }

        // Обнуляем параметры движения
        player.motionX = 0;
        player.motionY = 0;
        player.motionZ = 0;
        player.fallDistance = 0;
        player.jumpMovementFactor = 0;
        player.setSprinting(false);
        player.setAir(300);

        // КРИТИЧЕСКОЕ: предотвращаем обновление позиции через отражение
        try {
            Field firstUpdate = EntityPlayer.class.getDeclaredField("firstUpdate");
            firstUpdate.setAccessible(true);
            firstUpdate.setBoolean(player, true);
        } catch (Exception e) {
            // Игнорируем ошибки отражения - это не критично
        }
    }

    private boolean isPositionWithinTolerance(EntityPlayer player, String username) {
        double[] lastPos = LAST_VALID_POSITION.get(username);
        if (lastPos == null) return false;

        return Math.abs(player.posX - lastPos[0]) <= POSITION_CHECK_TOLERANCE &&
                Math.abs(player.posY - lastPos[1]) <= 0.5 &&
                Math.abs(player.posZ - lastPos[2]) <= POSITION_CHECK_TOLERANCE;
    }

    private void handlePlayerInteraction(PlayerInteractEvent event) {
        String username = normalizeUsername(event.entityPlayer.getCommandSenderName());

        // Обновляем время активности
        if (event.entityPlayer.ticksExisted % ACTIVITY_UPDATE_INTERVAL == 0) {
            updateLoginTime(username);
        }

        if (!isPlayerAuthenticated(event.entityPlayer)) {
            event.setCanceled(true);
            sendPrivateMessage(event.entityPlayer, "§cВы должны авторизоваться перед взаимодействием!");
        }
    }

    private void handlePlayerAttack(AttackEntityEvent event) {
        String username = normalizeUsername(event.entityPlayer.getCommandSenderName());

        // Обновляем время активности
        if (event.entityPlayer.ticksExisted % ACTIVITY_UPDATE_INTERVAL == 0) {
            updateLoginTime(username);
        }

        if (!isPlayerAuthenticated(event.entityPlayer)) {
            event.setCanceled(true);
            sendPrivateMessage(event.entityPlayer, "§cВы должны авторизоваться перед атакой!");
        }
    }

    private void handleItemPickup(EntityItemPickupEvent event) {
        String username = normalizeUsername(event.entityPlayer.getCommandSenderName());

        // Обновляем время активности
        if (event.entityPlayer.ticksExisted % ACTIVITY_UPDATE_INTERVAL == 0) {
            updateLoginTime(username);
        }

        if (!isPlayerAuthenticated(event.entityPlayer)) {
            event.setCanceled(true);
            sendPrivateMessage(event.entityPlayer, "§cВы должны авторизоваться перед подбором предметов!");
        }
    }

    private void handleItemToss(ItemTossEvent event) {
        String username = normalizeUsername(event.player.getCommandSenderName());

        // Обновляем время активности
        if (event.player.ticksExisted % ACTIVITY_UPDATE_INTERVAL == 0) {
            updateLoginTime(username);
        }

        if (!isPlayerAuthenticated(event.player)) {
            event.setCanceled(true);
            sendPrivateMessage(event.player, "§cВы должны авторизоваться перед выкидыванием предметов!");

            // Явно возвращаем предмет в инвентарь
            if (event.entityItem != null && event.entityItem.getEntityItem() != null) {
                event.player.inventory.addItemStackToInventory(event.entityItem.getEntityItem());
            }
        }
    }

    /**
     * Очищает данные игрока при выходе
     */
    public static void clearPlayerData(String username) {
        AUTHENTICATED_PLAYERS.remove(username);
        LOGIN_TIME_MAP.remove(username);
        LAST_VALID_POSITION.remove(username);
        LOGIN_TICK_COUNTER.remove(username);
        POSITION_INITIALIZED.remove(username);
    }

    private static void sendPrivateMessage(EntityPlayer player, String message) {
        if (player == null || player.worldObj.isRemote) return;

        // Проверка, что соединение с игроком активно
        if (player instanceof EntityPlayerMP) {
            EntityPlayerMP playerMP = (EntityPlayerMP) player;
            if (playerMP.playerNetServerHandler == null) {
                // Соединение закрыто, не пытаемся отправлять сообщения
                return;
            }
        }

        try {
            player.addChatMessage(new ChatComponentText(message));
        } catch (Exception e) {
            // Игнорируем ошибки отправки сообщений
            AuthMod.logger.debug("Failed to send message to player " + player.getCommandSenderName() + ": " + e.getMessage());
        }
    }
}