package com.example.authmod;

import cpw.mods.fml.common.FMLCommonHandler;
import cpw.mods.fml.common.eventhandler.SubscribeEvent;
import cpw.mods.fml.common.gameevent.PlayerEvent;
import cpw.mods.fml.common.gameevent.TickEvent;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.event.ClickEvent;
import net.minecraft.event.HoverEvent;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.management.ServerConfigurationManager;
import net.minecraft.util.ChatComponentText;
import net.minecraft.util.ChatStyle;
import net.minecraft.util.IChatComponent;
import net.minecraft.world.World;
import net.minecraftforge.event.entity.item.ItemTossEvent;
import net.minecraftforge.event.entity.living.LivingHurtEvent;
import net.minecraftforge.event.entity.player.AttackEntityEvent;
import net.minecraftforge.event.entity.player.EntityItemPickupEvent;
import net.minecraftforge.event.entity.player.PlayerInteractEvent;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.concurrent.*;

public class AuthEventHandler {

    public static final long MAX_LOGIN_TIME = 3 * 60 * 1000;
    private static final Map<String, ScheduledFuture<?>> KICK_TASKS = new ConcurrentHashMap<>();

    private static final int CHECK_INTERVAL = 1;

    private static final double POSITION_TOLERANCE = 0.1; // Уменьшено

    private static final double POSITION_CHECK_TOLERANCE = 0.05;

    private static final int ACTIVITY_UPDATE_INTERVAL = 20;

    private static final Map<String, double[]> LAST_VALID_POSITION = new ConcurrentHashMap<>();

    private static final Map<String, Integer> LOGIN_TICK_COUNTER = new ConcurrentHashMap<>();

    private static final Map<String, Boolean> POSITION_INITIALIZED = new ConcurrentHashMap<>();

    private static final Map<String, Boolean> AUTHENTICATED_PLAYERS = new ConcurrentHashMap<>();

    private static final Map<String, Long> LOGIN_TIME_MAP = new ConcurrentHashMap<>();

    private static final ExecutorService LOGIN_MESSAGE_EXECUTOR = Executors.newFixedThreadPool(2, r -> {
        Thread t = new Thread(r);
        t.setName("Auth-Login-Messages");
        t.setDaemon(true);
        return t;
    });

    private static final ScheduledExecutorService SCHEDULER = Executors.newSingleThreadScheduledExecutor(r -> {
        Thread t = new Thread(r);
        t.setName("Auth-Timeout-Scheduler");
        t.setDaemon(true);
        return t;
    });

    public static String normalizeUsername(String username) {
        return (username != null) ? username : "unknown";
    }

    public static boolean isPlayerAuthenticated(EntityPlayer player) {
        if (player == null) return false;

        String username = normalizeUsername(player.getCommandSenderName());
        return Boolean.TRUE.equals(AUTHENTICATED_PLAYERS.get(username));
    }

    public static void authenticatePlayer(EntityPlayer player) {
        if (player == null) return;

        String username = normalizeUsername(player.getCommandSenderName());

        AuthEventHandler.cancelKickTask(username);

        AUTHENTICATED_PLAYERS.put(username, true);
        LOGIN_TIME_MAP.remove(username);
        LAST_VALID_POSITION.remove(username);
        POSITION_INITIALIZED.remove(username);

        if (player instanceof EntityPlayerMP) {
            String ip = getPlayerIP((EntityPlayerMP) player);
            PlayerDataManager.updateLoginData(username, ip);

            AuthMod.logger.info("[OP_DEBUG] About to call reopPlayerOnServer for: {}", username);
            reopPlayerOnServer((EntityPlayerMP) player);

        }
    }

    public static void updateLoginTime(String username) {
        String normalizedUsername = normalizeUsername(username);
        if (LOGIN_TIME_MAP.containsKey(normalizedUsername)) {
            LOGIN_TIME_MAP.put(normalizedUsername, System.currentTimeMillis());
        }
    }

    public static String getPlayerIP(EntityPlayerMP player) {
        try {
            String ip = player.getPlayerIP();

            if (ip.contains(":")) {
                return ip.substring(0, ip.indexOf(':'));
            }
            return ip;
        } catch (Exception e) {
            return "unknown";
        }
    }

    public static void shutdown() {
        LOGIN_MESSAGE_EXECUTOR.shutdown();
    }

    public static void initializePlayerPosition(EntityPlayer player, String username) {
        World world = player.worldObj;

        int x = (int) Math.floor(player.posX);
        int y = (int) Math.floor(player.posY);
        int z = (int) Math.floor(player.posZ);

        double safeY = y + 1.0;

        if (world.getBlock(x, (int) safeY, z).getMaterial().isSolid()) {
            safeY = y + 2.0; // Если блок занят, поднимаем выше
        }

        double[] position = {player.posX, safeY, player.posZ};
        LAST_VALID_POSITION.put(username, position);

        player.setPositionAndUpdate(position[0], position[1], position[2]);
    }

    public static void deauthenticatePlayer(EntityPlayer player) {
        if (player == null) return;

        String username = normalizeUsername(player.getCommandSenderName());
        clearPlayerData(username);
        AUTHENTICATED_PLAYERS.put(username, false);
        LOGIN_TIME_MAP.put(username, System.currentTimeMillis());

        if (player instanceof EntityPlayerMP) {
            initializePlayerPosition(player, username);

            deopPlayerOnServer((EntityPlayerMP) player);

        }
    }

    public static void clearPlayerData(String username) {
        AUTHENTICATED_PLAYERS.remove(username);
        LOGIN_TIME_MAP.remove(username);
        LAST_VALID_POSITION.remove(username);
        LOGIN_TICK_COUNTER.remove(username);
        POSITION_INITIALIZED.remove(username);
    }

    private static void sendPrivateMessage(EntityPlayer player, String message) {
        if (player == null || player.worldObj.isRemote) return;

        if (player instanceof EntityPlayerMP) {
            EntityPlayerMP playerMP = (EntityPlayerMP) player;
            if (playerMP.playerNetServerHandler == null) {

                return;
            }
        }

        try {
            player.addChatMessage(new ChatComponentText(message));
        } catch (Exception e) {

            AuthMod.logger.debug("Failed to send message to player {}: {}", player.getCommandSenderName(), e.getMessage());
        }
    }

    public static void deopPlayerOnServer(EntityPlayerMP player) {
        if (player == null) return;
        MinecraftServer server = FMLCommonHandler.instance().getMinecraftServerInstance();
        if (server != null) {
            ServerConfigurationManager configManager = server.getConfigurationManager();
            if (configManager != null) {
                String username = player.getCommandSenderName();

                if (configManager.func_152596_g(player.getGameProfile())) { // isPlayerOpped
                    AuthMod.logger.info("Deopping player on server: {}", username);
                    configManager.func_152610_b(player.getGameProfile()); // removePlayerFromOppedPlayers


                }
            }
        }
    }

    public static void reopPlayerOnServer(EntityPlayerMP player) {
        if (player == null) return;
        String username = normalizeUsername(player.getCommandSenderName());

        AuthMod.logger.info("[OP_DEBUG] Checking isPlayerOperator for: {}. Result: {}", username, PlayerDataManager.isPlayerOperator(username)); // <--- Лог
        if (PlayerDataManager.isPlayerOperator(username)) { // <--- Эта проверка
            MinecraftServer server = FMLCommonHandler.instance().getMinecraftServerInstance();
            if (server != null) {
                ServerConfigurationManager configManager = server.getConfigurationManager();
                if (configManager != null) {

                    if (!configManager.func_152596_g(player.getGameProfile())) { // !isPlayerOpped
                        AuthMod.logger.info("Re-Opping player on server: {}", username);
                        configManager.func_152605_a(player.getGameProfile()); // addPlayerToOppedPlayers

                        sendPrivateMessage(player, "§aСтатус оператора был восстановлен");
                    } else {
                        AuthMod.logger.info("[OP_DEBUG] Player {} is already Opped on server.", username);
                    }
                } else {
                    AuthMod.logger.warn("[OP_DEBUG] ServerConfigurationManager is null in reopPlayerOnServer for: {}", username);
                }
            } else {
                AuthMod.logger.warn("[OP_DEBUG] MinecraftServer instance is null in reopPlayerOnServer for: {}", username);
            }
        } else {
            AuthMod.logger.debug("Player " + username + " is not marked as an operator in data, skipping re-op.");
        }
    }

    @SubscribeEvent
    public void onPlayerLogin(PlayerEvent.PlayerLoggedInEvent event) {
        handlePlayerLogin(event.player);
    }

    @SubscribeEvent
    public void onPlayerLogout(PlayerEvent.PlayerLoggedOutEvent event) {
        String username = normalizeUsername(event.player.getCommandSenderName());
        cancelKickTask(username);
        clearPlayerData(username);
    }

    @SubscribeEvent
    public void onPlayerTick(TickEvent.PlayerTickEvent event) {

        if (event.side.isServer() && event.phase == TickEvent.Phase.START && event.player instanceof EntityPlayer) {
            handlePlayerMovement(event.player);
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

        if (event.entityLiving instanceof EntityPlayer) {
            EntityPlayer player = (EntityPlayer) event.entityLiving;

            if (!isPlayerAuthenticated(player)) {

                event.setCanceled(true);


            }
        }
    }

    private void handlePlayerLogin(EntityPlayer player) {
        String username = normalizeUsername(player.getCommandSenderName());

        cancelKickTask(username);

        AUTHENTICATED_PLAYERS.put(username, false);
        LOGIN_TIME_MAP.put(username, System.currentTimeMillis());
        LOGIN_TICK_COUNTER.put(username, 0);
        POSITION_INITIALIZED.put(username, false);

        if (player instanceof EntityPlayerMP) {
            deopPlayerOnServer((EntityPlayerMP) player);
        }


        initializePlayerPosition(player, username);
        POSITION_INITIALIZED.put(username, true);

        scheduleKickTask(username);

        scheduleLoginMessage(player, username);

        AuthMod.logger.info("Player login initialized: {}", username);
    }

    private void scheduleKickTask(String username) {
        cancelKickTask(username);

        ScheduledFuture<?> kickTask = SCHEDULER.schedule(() -> {
            if (Boolean.FALSE.equals(AUTHENTICATED_PLAYERS.get(username))) {
                EntityPlayerMP playerMP = getOnlinePlayerByName(username);
                if (playerMP != null) {
                    try {
                        playerMP.playerNetServerHandler.kickPlayerFromServer("Время на авторизацию истекло!");
                        AuthMod.logger.info("Player {} kicked due to timeout", username);
                    } catch (Exception e) {
                        AuthMod.logger.warn("Failed to kick player {}: {}", username, e.getMessage());
                    }
                }
            }
            KICK_TASKS.remove(username);
        }, MAX_LOGIN_TIME, TimeUnit.MILLISECONDS);

        KICK_TASKS.put(username, kickTask);
    }

    private static void cancelKickTask(String username) {
        ScheduledFuture<?> task = KICK_TASKS.remove(username);
        if (task != null) {
            task.cancel(false);
        }
    }

    private EntityPlayerMP getOnlinePlayerByName(String username) {
        MinecraftServer server = FMLCommonHandler.instance().getMinecraftServerInstance();
        if (server != null) {
            ServerConfigurationManager configManager = server.getConfigurationManager();
            if (configManager != null) {
                for (Object playerObj : (Iterable<?>) configManager.playerEntityList) {
                    if (playerObj instanceof EntityPlayerMP) {
                        EntityPlayerMP playerMP = (EntityPlayerMP) playerObj;
                        if (playerMP.getCommandSenderName().equalsIgnoreCase(username)) {
                            return playerMP;
                        }
                    }
                }
            }
        }
        return null;
    }

    private void scheduleLoginMessage(EntityPlayer player, String username) {
        LOGIN_MESSAGE_EXECUTOR.submit(() -> {
            try {
                Thread.sleep(2000);
                sendLoginInstructions(player, username);
            } catch (InterruptedException e) {
                AuthMod.logger.error("Failed to send login message to {}", username, e);
            }
        });
    }

    private IChatComponent createSeparator() {
        return new ChatComponentText("§8─────────────────────────────────────────────");
    }

    private IChatComponent createHeader(String title, String icon) {
        ChatComponentText header = new ChatComponentText(String.format("§6%s %s §6%s", icon, title, icon));
        header.setChatStyle(new ChatStyle().setBold(true));
        return header;
    }

    private void sendLoginInstructions(EntityPlayer player, String username) {
        if (player == null || player.worldObj.isRemote) return;
        if (Boolean.FALSE.equals(AUTHENTICATED_PLAYERS.get(username))) {
            // Отправляем заголовок
            player.addChatMessage(createHeader("Добро пожаловать на сервер!", "✦"));
            player.addChatMessage(createSeparator());

            if (PlayerDataManager.isPlayerRegistered(username)) {
                player.addChatMessage(new ChatComponentText("§eВы уже зарегистрированы!"));
                player.addChatMessage(new ChatComponentText("§eВведите команду для авторизации:"));

                // Команда для входа
                ChatComponentText loginCommand = new ChatComponentText("§7- §e/login <пароль> §7- войти в аккаунт");
                loginCommand.setChatStyle(new ChatStyle()
                        .setChatHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, new ChatComponentText("Нажмите для открытия формы входа")))
                        .setChatClickEvent(new ClickEvent(ClickEvent.Action.SUGGEST_COMMAND, "/auth login ")));
                player.addChatMessage(loginCommand);
            } else {
                player.addChatMessage(new ChatComponentText("§eВы первый раз на этом сервере?"));
                player.addChatMessage(new ChatComponentText("§eВам необходимо зарегистрировать аккаунт для защиты от взлома!"));

                // Команда для регистрации
                ChatComponentText registerCommand = new ChatComponentText("§7- §e/register <пароль> <подтверждение> §7- зарегистрировать аккаунт");
                registerCommand.setChatStyle(new ChatStyle()
                        .setChatHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, new ChatComponentText("Нажмите для открытия формы регистрации")))
                        .setChatClickEvent(new ClickEvent(ClickEvent.Action.SUGGEST_COMMAND, "/auth register ")));
                player.addChatMessage(registerCommand);

                player.addChatMessage(new ChatComponentText("§7- §fПример: §e/register MySecurePass123 MySecurePass123"));

                player.addChatMessage(createSeparator());
                player.addChatMessage(new ChatComponentText("§eТребования к паролю:"));
                player.addChatMessage(new ChatComponentText("§7- §fМинимум 8 символов"));
            }

            player.addChatMessage(createSeparator());
        }
    }

    private void handlePlayerMovement(EntityPlayer player) {
        String username = normalizeUsername(player.getCommandSenderName());

        if (player.ticksExisted % CHECK_INTERVAL != 0) {
            return;
        }

        updateLoginTickCounter(username, player);

        initializePlayerRecord(player, username);

        if (player.ticksExisted % ACTIVITY_UPDATE_INTERVAL == 0) {
            updateLoginTime(username);
        }

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

                if (!Boolean.TRUE.equals(POSITION_INITIALIZED.get(username))) {
                    initializePlayerPosition(player, username);
                    POSITION_INITIALIZED.put(username, true);
                    AuthMod.logger.info("Position initialized for {} at ({}, {}, {})", username, player.posX, player.posY, player.posZ);
                }
            }
        }
    }

    private void initializePlayerRecord(EntityPlayer player, String username) {
        if (!AUTHENTICATED_PLAYERS.containsKey(username)) {
            AuthMod.logger.warn("Player has no auth record, treating as unauthenticated: {}", username);
            AUTHENTICATED_PLAYERS.put(username, false);
            LOGIN_TIME_MAP.put(username, System.currentTimeMillis());
            initializePlayerPosition(player, username);
        }
    }

    private void handleUnauthorizedPlayer(EntityPlayer player, String username) {

        handleUnauthorizedMovement(player, username);

        checkLoginTimeout(player, username);

        sendAuthMessages(player, username);
    }

    private void checkLoginTimeout(EntityPlayer player, String username) {
        Long loginTime = LOGIN_TIME_MAP.get(username);
        if (loginTime == null) return;

        long currentTime = System.currentTimeMillis();
        long timeSinceLogin = currentTime - loginTime;

        if (timeSinceLogin > MAX_LOGIN_TIME) {

            if (player instanceof EntityPlayerMP) {
                EntityPlayerMP playerMP = (EntityPlayerMP) player;
                playerMP.playerNetServerHandler.kickPlayerFromServer("Время на авторизацию истекло!");
            }
        }
    }

    private void sendAuthMessages(EntityPlayer player, String username) {
        Long loginTime = LOGIN_TIME_MAP.get(username);
        if (loginTime == null) return;

        long currentTime = System.currentTimeMillis();
        long timeSinceLogin = currentTime - loginTime;
        long timeLeft = (MAX_LOGIN_TIME - timeSinceLogin) / 1000;

        if (timeLeft < 0) timeLeft = 0;

        // Если осталось меньше 60 секунд
        if (timeLeft > 0 && timeLeft <= 60 && player.ticksExisted % 100 == 0) {
            // Красный цвет при остатке менее 10 секунд
            String color = (timeLeft <= 10) ? "§c" : "§e";
            sendPrivateMessage(player, String.format("%sВНИМАНИЕ! §fОсталось %d сек. для авторизации!",
                    color, timeLeft));
        }
        // Регулярные напоминания
        else if (player.ticksExisted % 200 == 0) {
            String command = PlayerDataManager.isPlayerRegistered(username) ?
                    "§b/auth login <пароль>" : "§b/auth register <пароль> <подтверждение>";
            sendPrivateMessage(player, "§6Требуется авторизация: " + command);
        }
    }

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


        } else {
            player.setPosition(lastPos[0], lastPos[1], lastPos[2]);
        }

        player.motionX = 0;
        player.motionY = 0;
        player.motionZ = 0;
        player.fallDistance = 0;
        player.jumpMovementFactor = 0;
        player.setSprinting(false);


        try {
            Field firstUpdate = EntityPlayer.class.getDeclaredField("firstUpdate");
            firstUpdate.setAccessible(true);
            firstUpdate.setBoolean(player, true);
        } catch (Exception ignored) {

        }
    }

    private boolean isPositionWithinTolerance(EntityPlayer player, String username) {
        double[] lastPos = LAST_VALID_POSITION.get(username);
        if (lastPos == null) return false;

        return Math.abs(player.posX - lastPos[0]) <= POSITION_CHECK_TOLERANCE && Math.abs(player.posY - lastPos[1]) <= 0.5 && Math.abs(player.posZ - lastPos[2]) <= POSITION_CHECK_TOLERANCE;
    }

    private void handlePlayerInteraction(PlayerInteractEvent event) {
        String username = normalizeUsername(event.entityPlayer.getCommandSenderName());

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

        if (event.player.ticksExisted % ACTIVITY_UPDATE_INTERVAL == 0) {
            updateLoginTime(username);
        }

        if (!isPlayerAuthenticated(event.player)) {
            event.setCanceled(true);
            sendPrivateMessage(event.player, "§cВы должны авторизоваться перед выкидыванием предметов!");

            if (event.entityItem != null && event.entityItem.getEntityItem() != null) {
                event.player.inventory.addItemStackToInventory(event.entityItem.getEntityItem());
            }
        }
    }
}