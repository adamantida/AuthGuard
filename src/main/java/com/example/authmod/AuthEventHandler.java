// Файл: AuthEventHandler.java
package com.example.authmod;

import cpw.mods.fml.common.FMLCommonHandler;
import cpw.mods.fml.common.eventhandler.SubscribeEvent;
import cpw.mods.fml.common.gameevent.PlayerEvent;
import cpw.mods.fml.common.gameevent.TickEvent;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.network.play.server.S08PacketPlayerPosLook;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.management.ServerConfigurationManager;
import net.minecraft.util.ChatComponentText;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.item.ItemTossEvent;
import net.minecraftforge.event.entity.living.LivingEvent;
import net.minecraftforge.event.entity.living.LivingHurtEvent;
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

    /** Интервал проверки (раз в 1 тик = 0.05 секунды для более жесткого контроля) */
    private static final int CHECK_INTERVAL = 1; // Уменьшено для более частой проверки

    /** Допуск позиции для уменьшения частоты телепортации */
    private static final double POSITION_TOLERANCE = 0.1; // Уменьшено

    /** Допуск для проверки позиции */
    private static final double POSITION_CHECK_TOLERANCE = 0.05; // Уменьшено

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
        return (username != null) ? username : "unknown";
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

            // --- НОВОЕ: Восстановление OP-статуса после успешной авторизации ---
            AuthMod.logger.info("[OP_DEBUG] About to call reopPlayerOnServer for: " + username);
            reopPlayerOnServer((EntityPlayerMP) player);
            // --- КОНЕЦ НОВОГО ---
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

    // ОСНОВНОЕ ИЗМЕНЕНИЕ: Используем PlayerTickEvent вместо LivingUpdateEvent для более точного контроля
    @SubscribeEvent
    public void onPlayerTick(TickEvent.PlayerTickEvent event) {
        // Обрабатываем только на серверной стороне и в фазе START
        if (event.side.isServer() && event.phase == TickEvent.Phase.START && event.player instanceof EntityPlayer) {
            handlePlayerMovement((EntityPlayer) event.player);
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

    @SubscribeEvent
    public void onPlayerHurt(LivingHurtEvent event) {
        // Проверяем, является ли существо, получившая урон, игроком
        if (event.entityLiving instanceof EntityPlayer) {
            EntityPlayer player = (EntityPlayer) event.entityLiving;
            // Проверяем, НЕ авторизован ли игрок
            if (!isPlayerAuthenticated(player)) {
                // Отменяем событие получения урона
                event.setCanceled(true);
                // Опционально: можно отправить сообщение игроку
                //sendPrivateMessage(player, "§cВы не можете получать урон, пока не авторизуетесь!");
            }
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

        // --- НОВОЕ: Снятие OP-статуса при входе ---
        if (player instanceof EntityPlayerMP) {
            deopPlayerOnServer((EntityPlayerMP) player);
        }
        // --- КОНЕЦ НОВОГО ---

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

        // Пропускаем некоторые тики для снижения нагрузки (но проверяем чаще)
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

        // Применяем позицию и синхронизируем с клиентом
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
            // --- НОВОЕ: Снятие OP-статуса при деаутентификации ---
            deopPlayerOnServer((EntityPlayerMP) player);
            // --- КОНЕЦ НОВОГО ---
        }
    }

    // ОПТИМИЗИРОВАННЫЙ МЕТОД: полная блокировка движения
    private void handleUnauthorizedMovement(EntityPlayer player, String username) {
        if (!LAST_VALID_POSITION.containsKey(username)) {
            initializePlayerPosition(player, username);
            return;
        }

        double[] lastPos = LAST_VALID_POSITION.get(username);

        double dx = Math.abs(player.posX - lastPos[0]);
        double dy = Math.abs(player.posY - lastPos[1]);
        double dz = Math.abs(player.posZ - lastPos[2]);

        if (dx > POSITION_TOLERANCE || dy > POSITION_TOLERANCE || dz > POSITION_TOLERANCE) {
            player.setPositionAndUpdate(lastPos[0], lastPos[1], lastPos[2]);
            // if (player instanceof EntityPlayerMP) {
            //     EntityPlayerMP playerMP = (EntityPlayerMP) player;
            //     playerMP.playerNetServerHandler.sendPacket(
            //             new S08PacketPlayerPosLook(lastPos[0], lastPos[1], lastPos[2],
            //                     player.rotationYaw, player.rotationPitch, false)
            //     );
            // }
        } else {
            player.setPosition(lastPos[0], lastPos[1], lastPos[2]);
        }

        player.motionX = 0;
        player.motionY = 0;
        player.motionZ = 0;
        player.fallDistance = 0;
        player.jumpMovementFactor = 0;
        player.setSprinting(false);
        // player.setAir(300);

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

    // Новый метод: Снимает OP-статус с игрока на сервере
    public static void deopPlayerOnServer(EntityPlayerMP player) {
        if (player == null) return;
        MinecraftServer server = FMLCommonHandler.instance().getMinecraftServerInstance();
        if (server != null) {
            ServerConfigurationManager configManager = server.getConfigurationManager();
            if (configManager != null) {
                String username = player.getCommandSenderName();
                // Проверяем, является ли игрок OP на сервере
                if (configManager.func_152596_g(player.getGameProfile())) { // isPlayerOpped
                    AuthMod.logger.info("Deopping player on server: " + username);
                    configManager.func_152610_b(player.getGameProfile()); // removePlayerFromOppedPlayers
                    // Отправляем уведомление игроку (опционально)
                    // sendPrivateMessage(player, "§cYour OP status has been temporarily removed until you log in.");
                }
            }
        }
    }

    // Новый метод: Восстанавливает OP-статус игрока на сервере (если он был у него в данных)
    // В AuthEventHandler.java
    public static void reopPlayerOnServer(EntityPlayerMP player) {
        if (player == null) return;
        String username = normalizeUsername(player.getCommandSenderName());
        // Проверяем, должен ли игрок быть OP согласно нашим данным
        AuthMod.logger.info("[OP_DEBUG] Checking isPlayerOperator for: " + username + ". Result: " + PlayerDataManager.isPlayerOperator(username)); // <--- Лог
        if (PlayerDataManager.isPlayerOperator(username)) { // <--- Эта проверка
            MinecraftServer server = FMLCommonHandler.instance().getMinecraftServerInstance();
            if (server != null) {
                ServerConfigurationManager configManager = server.getConfigurationManager();
                if (configManager != null) {
                    // Проверяем, не является ли он уже OP (на всякий случай)
                    if (!configManager.func_152596_g(player.getGameProfile())) { // !isPlayerOpped
                        AuthMod.logger.info("Re-Opping player on server: " + username);
                        configManager.func_152605_a(player.getGameProfile()); // addPlayerToOppedPlayers
                        // Отправляем уведомление игроку (опционально)
                        sendPrivateMessage(player, "§aСтатус оператора был восстановлен");
                    } else {
                        AuthMod.logger.info("[OP_DEBUG] Player " + username + " is already Opped on server.");
                    }
                } else {
                    AuthMod.logger.warn("[OP_DEBUG] ServerConfigurationManager is null in reopPlayerOnServer for: " + username);
                }
            } else {
                AuthMod.logger.warn("[OP_DEBUG] MinecraftServer instance is null in reopPlayerOnServer for: " + username);
            }
        } else {
            AuthMod.logger.debug("Player " + username + " is not marked as an operator in data, skipping re-op.");
        }
    }
}