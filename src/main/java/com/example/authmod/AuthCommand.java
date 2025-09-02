package com.example.authmod;

import net.minecraft.command.CommandBase;
import net.minecraft.command.ICommandSender;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.event.ClickEvent;
import net.minecraft.util.ChatComponentText;
import net.minecraft.util.ChatStyle;
import net.minecraft.util.EnumChatFormatting;
import net.minecraft.util.IChatComponent;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Команда аутентификации для регистрации и входа игроков.
 * Предоставляет безопасный механизм авторизации на сервере.
 */
public class AuthCommand extends CommandBase {

    /**
     * Допустимые алиасы команды
     */
    private static final List<String> ALIASES = Arrays.asList("auth", "a");


    @Override
    public String getCommandName() {
        return "auth";
    }

    @Override
    public List<String> getCommandAliases() {
        return ALIASES;
    }

    @Override
    public String getCommandUsage(ICommandSender sender) {
        return "/auth <register|login|logout|changepassword> <аргументы>";
    }


    @Override
    public void processCommand(ICommandSender sender, String[] args) {

        if (!(sender instanceof EntityPlayer)) {
            return;
        }

        EntityPlayer player = (EntityPlayer) sender;
        String username = AuthEventHandler.normalizeUsername(player.getCommandSenderName());


        AuthEventHandler.updateLoginTime(username);


        if (args.length < 1) {
            sendUsageMessage(player);
            return;
        }


        processAuthAction(player, username, args);
    }

    /**
     * Отправляет сообщение с использованием команды
     */
    private void sendUsageMessage(EntityPlayer player) {
        sendMessage(player, "§cИспользование: /auth <register|login|logout|changepassword|admin>");
        sendMessage(player, "§eКраткие команды: /a r, /a l, /a out");
    }

    /**
     * Обрабатывает действие регистрации или входа
     */
    private void processAuthAction(EntityPlayer player, String username, String[] args) {
        if (args.length < 1) {
            sendUsageMessage(player);
            return;
        }

        String action = args[0].toLowerCase();


        try {
            switch (action) {
                case "register":
                case "r":
                    handleRegistration(player, username, Arrays.copyOfRange(args, 1, args.length));
                    break;
                case "login":
                case "l":
                    handleLogin(player, username, Arrays.copyOfRange(args, 1, args.length));
                    break;
                case "logout":
                case "out":
                    handleLogout(player, username);
                    break;
                case "changepassword":
                    handleChangePassword(player, username, Arrays.copyOfRange(args, 1, args.length));
                    break;
                case "admin":
                    handleAdminCommand(player, username, Arrays.copyOfRange(args, 1, args.length));
                    break;
                default:
                    sendUnknownCommandMessage(player);
            }
        } catch (Exception e) {
            handleCommandError(player, e);
        }
    }

    @Override
    public List<?> addTabCompletionOptions(ICommandSender sender, String[] args) {
        if (args.length == 1) {
            return getListOfStringsMatchingLastWord(args, "register", "r", "login", "l", "logout", "out", "changepassword", "admin");
        } else if (args.length == 2 && args[0].equalsIgnoreCase("admin")) {
            return getListOfStringsMatchingLastWord(args, "reset", "list", "ip", "add", "reload");
        } else if (args.length == 3 && args[0].equalsIgnoreCase("admin")) {
            // Автозаполнение ников для reset, ip, add
            if (Arrays.asList("reset", "ip", "add").contains(args[1].toLowerCase())) {
                // Получаем список зарегистрированных игроков из PlayerDataManager
                List<String> playerNames = PlayerDataManager.getAllPlayerNames();
                return getListOfStringsMatchingLastWord(args, playerNames.toArray(new String[0]));
            }
            // Фильтры для list
            else if (args[1].equalsIgnoreCase("list")) {
                return getListOfStringsMatchingLastWord(args, "all", "banned", "5min", "15min", "30min", "60min");
            }
        }
        return null;
    }


    private void handleRegistration(EntityPlayer player, String username, String[] args) {

        if (args.length < 2) {
            sendPasswordConfirmationRequired(player);
            return;
        }

        if (PlayerDataManager.isPlayerRegistered(username)) {
            sendAlreadyRegisteredMessage(player);
            return;
        }


        if (!doPasswordsMatch(args[0], args[1])) {
            sendPasswordsDoNotMatch(player);
            return;
        }


        if (isPasswordTooShort(args[0])) {
            sendPasswordTooShort(player);
            return;
        }


        registerPlayer(player, username, args[0]);
    }

    private void handleLogin(EntityPlayer player, String username, String[] args) {
        if (args.length < 1) {
            sendIncorrectPasswordMessage(player);
            return;
        }

        String password = args[0];

        if (!PlayerDataManager.isPlayerRegistered(username)) {
            sendNotRegisteredMessage(player);
            return;
        }


        if (AuthHelper.verifyPassword(password, PlayerDataManager.getPlayerHash(username))) {
            if (player instanceof EntityPlayerMP) {
                String ip = AuthEventHandler.getPlayerIP((EntityPlayerMP) player);
                PlayerDataManager.updateLoginData(username, ip);
            }
            AuthEventHandler.authenticatePlayer(player);
            sendLoginSuccessMessage(player);
        } else {
            sendIncorrectPasswordMessage(player);
        }
    }

    private void handleLogout(EntityPlayer player, String username) {
        if (!AuthEventHandler.isPlayerAuthenticated(player)) {
            sendNotLoggedInMessage(player);
            return;
        }


        AuthEventHandler.deauthenticatePlayer(player);

        sendLogoutSuccessMessage(player);
    }

    private void handleChangePassword(EntityPlayer player, String username, String[] args) {
        if (!AuthEventHandler.isPlayerAuthenticated(player)) {
            sendNotLoggedInMessage(player);
            return;
        }

        if (args.length < 3) {
            sendChangePasswordUsage(player);
            return;
        }

        String oldPassword = args[0];
        String newPassword = args[1];
        String confirmPassword = args[2];


        if (!AuthHelper.verifyPassword(oldPassword, PlayerDataManager.getPlayerHash(username))) {
            sendIncorrectOldPasswordMessage(player);
            return;
        }


        if (!newPassword.equals(confirmPassword)) {
            sendPasswordsDoNotMatch(player);
            return;
        }

        if (isPasswordTooShort(newPassword)) {
            sendPasswordTooShort(player);
            return;
        }


        if (PlayerDataManager.updatePassword(username, newPassword)) {
            sendPasswordChangedSuccess(player);
        } else {
            sendCommandError(player, "Ошибка изменения пароля");
        }
    }

    private void sendCommandError(EntityPlayer player, String message) {
        sendMessage(player, "§c" + message);
    }

    private void handleAdminCommand(EntityPlayer player, String username, String[] args) {

        if (!player.canCommandSenderUseCommand(4, "auth.admin")) {
            sendNoPermissionMessage(player);
            return;
        }

        if (args.length < 1) {
            sendAdminUsageMessage(player);
            return;
        }

        String adminAction = args[0].toLowerCase();

        switch (adminAction) {
            case "reset":
                if (args.length < 2) {
                    sendAdminResetUsage(player);
                    return;
                }
                handleAdminReset(player, args[1]);
                break;
            case "list":

                String filter = "all";
                if (args.length > 1) {
                    filter = args[1].toLowerCase();
                }
                handleAdminList(player, filter, 1);
                break;
            case "ip":
                if (args.length < 2) {
                    sendAdminIPUsage(player);
                    return;
                }
                handleAdminIP(player, args[1]);
                break;
            case "add":
                if (args.length < 2) {
                    sendAdminAddUsage(player);
                    return;
                }
                handleAdminAddOp(player, args[1]);
                break;

            case "reload":
                handleAdminReload(player);
                break;
            default:
                sendAdminUsageMessage(player);
        }
    }

    private boolean doPasswordsMatch(String password, String confirmation) {
        return password.equals(confirmation);
    }

    private boolean isPasswordTooShort(String password) {
        return password.length() < 8;
    }

    private void registerPlayer(EntityPlayer player, String username, String password) {
        if (player instanceof EntityPlayerMP) {
            String ip = AuthEventHandler.getPlayerIP((EntityPlayerMP) player);
            PlayerDataManager.registerPlayer(username, password, ip);
        } else {
            PlayerDataManager.registerPlayer(username, password, "unknown");
        }

        AuthEventHandler.authenticatePlayer(player);
        sendRegistrationSuccessMessage(player);
    }

    private void sendUnknownCommandMessage(EntityPlayer player) {
        sendMessage(player, "§cНеизвестная команда! Используйте register, login, logout, changepassword или admin");
    }

    private void sendPasswordConfirmationRequired(EntityPlayer player) {
        sendMessage(player, "§cДля регистрации нужно подтверждение пароля");
    }

    private void sendAlreadyRegisteredMessage(EntityPlayer player) {
        sendMessage(player, "§cВы уже зарегистрированы!");
    }

    private void sendPasswordsDoNotMatch(EntityPlayer player) {
        sendMessage(player, "§cПароли не совпадают!");
    }

    private void sendPasswordTooShort(EntityPlayer player) {
        sendMessage(player, "§cПароль должен содержать минимум 8 символов!");
    }

    private void sendNotRegisteredMessage(EntityPlayer player) {
        sendMessage(player, "§cСначала нужно зарегистрироваться!");
    }

    private void sendIncorrectPasswordMessage(EntityPlayer player) {
        sendMessage(player, "§cНеверный пароль!");
    }

    private void sendLoginSuccessMessage(EntityPlayer player) {
        sendMessage(player, "§aАвторизация успешна!");
    }

    private void sendRegistrationSuccessMessage(EntityPlayer player) {
        sendMessage(player, "§aРегистрация успешна! Вы авторизованы.");
    }

    private void sendLogoutSuccessMessage(EntityPlayer player) {
        sendMessage(player, "§aВы успешно вышли из системы.");
    }

    private void sendNotLoggedInMessage(EntityPlayer player) {
        sendMessage(player, "§cВы не авторизованы!");
    }

    private void sendChangePasswordUsage(EntityPlayer player) {
        sendMessage(player, "§cИспользование: /auth changepassword <старый> <новый> <подтверждение>");
    }

    private void sendIncorrectOldPasswordMessage(EntityPlayer player) {
        sendMessage(player, "§cНеверный текущий пароль!");
    }

    private void sendPasswordChangedSuccess(EntityPlayer player) {
        sendMessage(player, "§aПароль успешно изменен!");
    }

    private void sendAdminUsageMessage(EntityPlayer player) {
        sendMessage(player, "§6Админ-команды:");
        sendMessage(player, "§e/auth admin reset <игрок> §7- сбросить пароль");
        sendMessage(player, "§e/auth admin ip <игрок> §7- информация об IP");

        sendMessage(player, "§e/auth admin add <игрок> §7- добавить игрока в список операторов мода");
        sendMessage(player, "§e/auth admin reload §7- перезагрузить данные мода из файла");

        sendMessage(player, "§e/auth admin list [all|banned|5min|15min|30min|60min] §7- список игроков");
    }

    private void sendAdminResetUsage(EntityPlayer player) {
        sendMessage(player, "§cИспользование: /auth admin reset <игрок>");
    }

    private void sendAdminIPUsage(EntityPlayer player) {
        sendMessage(player, "§cИспользование: /auth admin ip <игрок>");
    }

    private void sendNoPermissionMessage(EntityPlayer player) {
        sendMessage(player, "§cУ вас недостаточно прав для выполнения этой команды!");
    }

    private void handleAdminReset(EntityPlayer player, String targetUsername) {
        if (PlayerDataManager.resetPlayerPassword(targetUsername)) {
            sendMessage(player, "§aПароль игрока " + targetUsername + " сброшен.");
        } else {
            sendMessage(player, "§cИгрок " + targetUsername + " не найден.");
        }
    }

    private void handleAdminIP(EntityPlayer player, String username) {
        PlayerData data = PlayerDataManager.getPlayerData(username);
        if (data == null) {
            sendMessage(player, "§cИгрок не найден.");
            return;
        }

        String banStatus = data.isBanned() ? "§cЗАБАНЕН" : "§aАКТИВЕН";

        sendMessage(player, String.format("§6Информация об игроке %s:", username));
        sendMessage(player, String.format("§eПоследний IP: %s", data.getLastLoginIP()));
        sendMessage(player, String.format("§eIP регистрации: %s", data.getRegistrationIP()));
        sendMessage(player, String.format("§eСтатус: %s", banStatus));
    }

    private void handleAdminList(EntityPlayer player, String filter, int page) {
        List<PlayerData> players = PlayerDataManager.getAllPlayers();

        // Используем applyFilter для фильтрации списка игроков
        players = applyFilter(players, filter);

        if (players.isEmpty()) {
            sendMessage(player, "§cНет игроков, соответствующих фильтру: " + filter);
            return;
        }

        final int pageSize = 10;
        int totalPages = (players.size() + pageSize - 1) / pageSize;
        if (page < 1) page = 1;
        if (page > totalPages) page = totalPages;
        int start = (page - 1) * pageSize;
        int end = Math.min(start + pageSize, players.size());

        List<PlayerData> pageData = players.subList(start, end);
        sendAdminListHeader(player, filter, page, totalPages);

        for (PlayerData data : pageData) {
            String banStatus = data.isBanned() ? "§cБ" : "§aА";
            String message = String.format("§e%s §7| §bП.IP: %s §7| §bР.IP: %s §7| §eСтат: %s",
                    data.getUsername(), data.getLastLoginIP(), data.getRegistrationIP(), banStatus);
            sendMessage(player, message);
        }

        sendPaginationControls(player, page, totalPages, filter);
        sendMessage(player, String.format("§6Страница %d/%d (Найдено: %d)", page, totalPages, players.size()));
    }

    private List<PlayerData> applyFilter(List<PlayerData> players, String filter) {
        long currentTime = System.currentTimeMillis();
        long fiveMinutesAgo = currentTime - 5 * 60 * 1000;
        long fifteenMinutesAgo = currentTime - 15 * 60 * 1000;
        long thirtyMinutesAgo = currentTime - 30 * 60 * 1000;
        long sixtyMinutesAgo = currentTime - 60 * 60 * 1000;

        switch (filter.toLowerCase()) {
            case "banned":
                return players.stream().filter(PlayerData::isBanned).collect(Collectors.toList());
            case "5min":
                return players.stream().filter(p -> p.getLastLoginDate() >= fiveMinutesAgo).collect(Collectors.toList());
            case "15min":
                return players.stream().filter(p -> p.getLastLoginDate() >= fifteenMinutesAgo).collect(Collectors.toList());
            case "30min":
                return players.stream().filter(p -> p.getLastLoginDate() >= thirtyMinutesAgo).collect(Collectors.toList());
            case "60min":
                return players.stream().filter(p -> p.getLastLoginDate() >= sixtyMinutesAgo).collect(Collectors.toList());
            default:
                return players;
        }
    }

    private void sendPaginationControls(EntityPlayer player, int currentPage, int totalPages, String filter) {
        IChatComponent controls = new ChatComponentText("§6Навигация: ");

        if (currentPage > 1) {
            ChatComponentText prevBtn = new ChatComponentText("[←] ");
            prevBtn.setChatStyle(new ChatStyle()
                    .setColor(EnumChatFormatting.YELLOW)
                    .setChatClickEvent(new ClickEvent(ClickEvent.Action.RUN_COMMAND,
                            String.format("/auth admin list %s %d", filter, currentPage - 1))));
            controls.appendSibling(prevBtn);
        }

        ChatComponentText currentPageText = new ChatComponentText(String.format("[%d/%d] ", currentPage, totalPages));
        currentPageText.setChatStyle(new ChatStyle().setColor(EnumChatFormatting.GREEN));
        controls.appendSibling(currentPageText);

        if (currentPage < totalPages) {
            ChatComponentText nextBtn = new ChatComponentText("[→]");
            nextBtn.setChatStyle(new ChatStyle()
                    .setColor(EnumChatFormatting.YELLOW)
                    .setChatClickEvent(new ClickEvent(ClickEvent.Action.RUN_COMMAND,
                            String.format("/auth admin list %s %d", filter, currentPage + 1))));
            controls.appendSibling(nextBtn);
        }

        player.addChatMessage(controls);
    }

    private void sendAdminListHeader(EntityPlayer player, String filter, int currentPage, int totalPages) {
        IChatComponent header = new ChatComponentText("§6");

        ChatComponentText allBtn = new ChatComponentText("[Все] ");
        allBtn.setChatStyle(new ChatStyle()
                .setColor(EnumChatFormatting.GREEN)
                .setChatClickEvent(new ClickEvent(ClickEvent.Action.RUN_COMMAND, "/auth admin list all")));

        ChatComponentText bannedBtn = new ChatComponentText("[Забаненные] ");
        bannedBtn.setChatStyle(new ChatStyle()
                .setColor(EnumChatFormatting.RED)
                .setChatClickEvent(new ClickEvent(ClickEvent.Action.RUN_COMMAND, "/auth admin list banned")));


        ChatComponentText last5min = new ChatComponentText("[5м] ");
        last5min.setChatStyle(new ChatStyle()
                .setColor(EnumChatFormatting.YELLOW)
                .setChatClickEvent(new ClickEvent(ClickEvent.Action.RUN_COMMAND, "/auth admin list 5min")));

        ChatComponentText last15min = new ChatComponentText("[15м] ");
        last15min.setChatStyle(new ChatStyle()
                .setColor(EnumChatFormatting.YELLOW)
                .setChatClickEvent(new ClickEvent(ClickEvent.Action.RUN_COMMAND, "/auth admin list 15min")));

        ChatComponentText last30min = new ChatComponentText("[30м] ");
        last30min.setChatStyle(new ChatStyle()
                .setColor(EnumChatFormatting.YELLOW)
                .setChatClickEvent(new ClickEvent(ClickEvent.Action.RUN_COMMAND, "/auth admin list 30min")));

        ChatComponentText last60min = new ChatComponentText("[60м] ");
        last60min.setChatStyle(new ChatStyle()
                .setColor(EnumChatFormatting.YELLOW)
                .setChatClickEvent(new ClickEvent(ClickEvent.Action.RUN_COMMAND, "/auth admin list 60min")));


        IChatComponent navigation = new ChatComponentText(" §7| Страницы: ");

        if (currentPage > 1) {
            ChatComponentText prevBtn = new ChatComponentText("[←] ");
            prevBtn.setChatStyle(new ChatStyle()
                    .setColor(EnumChatFormatting.YELLOW)
                    .setChatClickEvent(new ClickEvent(ClickEvent.Action.RUN_COMMAND,
                            String.format("/auth admin list %s %d", filter, currentPage - 1))));
            navigation.appendSibling(prevBtn);
        }


        ChatComponentText currentPageText = new ChatComponentText(String.format("[%d/%d] ", currentPage, totalPages));
        currentPageText.setChatStyle(new ChatStyle().setColor(EnumChatFormatting.GREEN));
        navigation.appendSibling(currentPageText);

        if (currentPage < totalPages) {
            ChatComponentText nextBtn = new ChatComponentText("[→]");
            nextBtn.setChatStyle(new ChatStyle()
                    .setColor(EnumChatFormatting.YELLOW)
                    .setChatClickEvent(new ClickEvent(ClickEvent.Action.RUN_COMMAND,
                            String.format("/auth admin list %s %d", filter, currentPage + 1))));
            navigation.appendSibling(nextBtn);
        }


        header.appendSibling(allBtn);
        header.appendSibling(bannedBtn);
        header.appendSibling(last5min);
        header.appendSibling(last15min);
        header.appendSibling(last30min);
        header.appendSibling(last60min);
        header.appendSibling(navigation);

        player.addChatMessage(header);
    }

    private void handleCommandError(EntityPlayer player, Exception e) {
        AuthMod.logger.error("Auth command error", e);
        sendMessage(player, "§cПроизошла ошибка при обработке команды");
    }

    private void sendMessage(EntityPlayer player, String message) {
        if (player != null) {
            player.addChatMessage(new ChatComponentText(message));
        }
    }

    private void handleAdminAddOp(EntityPlayer admin, String targetPlayerName) {

        if (!PlayerDataManager.isPlayerRegistered(targetPlayerName)) {

            sendCommandError(admin, "Игрок '" + targetPlayerName + "' не зарегистрирован в системе аутентификации.");
            AuthMod.logger.warn("[ADMIN] Admin {} tried to add unregistered player '{}' as operator.", admin.getCommandSenderName(), targetPlayerName);
            return;
        }

        if (PlayerDataManager.isPlayerOperator(targetPlayerName)) {
            sendCommandError(admin, "Игрок '" + targetPlayerName + "' уже является оператором в системе аутентификации.");
            return;
        }


        if (PlayerDataManager.updateOperatorStatus(targetPlayerName, true)) {
            sendMessage(admin, "§aИгрок '" + targetPlayerName + "' успешно добавлен в список операторов системы аутентификации.");

            EntityPlayerMP targetPlayerMP = getOnlinePlayerByName(targetPlayerName);
            if (targetPlayerMP != null) {
                AuthEventHandler.reopPlayerOnServer(targetPlayerMP);
                sendMessage(admin, "§aOP-статус также выдан игроку '" + targetPlayerName + "' на сервере (онлайн).");
                AuthMod.logger.info("[ADMIN] Admin {} added player '{}' as operator (data updated, server OP granted).", admin.getCommandSenderName(), targetPlayerName);
            } else {
                AuthMod.logger.info("[ADMIN] Admin {} added player '{}' as operator (data updated).", admin.getCommandSenderName(), targetPlayerName);
            }


        } else {
            sendCommandError(admin, "Ошибка при добавлении игрока '" + targetPlayerName + "' в список операторов.");
            AuthMod.logger.error("[ADMIN] Failed to add player '{}' as operator for admin {}.", targetPlayerName, admin.getCommandSenderName());
        }
    }

    private void handleAdminReload(EntityPlayer admin) {
        try {
            PlayerDataManager.reloadData();
            sendMessage(admin, "§aДанные системы аутентификации успешно перезагружены из файла.");
            AuthMod.logger.info("[ADMIN] Admin {} reloaded auth data.", admin.getCommandSenderName());
        } catch (Exception e) {
            sendCommandError(admin, "Ошибка при перезагрузке данных: " + e.getMessage());
            AuthMod.logger.error("[ADMIN] Admin {} failed to reload auth data.", admin.getCommandSenderName(), e);
        }
    }

    private EntityPlayerMP getOnlinePlayerByName(String username) {
        net.minecraft.server.MinecraftServer server = net.minecraft.server.MinecraftServer.getServer();
        if (server != null) {
            net.minecraft.server.management.ServerConfigurationManager configManager = server.getConfigurationManager();
            if (configManager != null) {
                for (Object playerObj : (java.lang.Iterable<?>) configManager.playerEntityList) {
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

    private void sendAdminAddUsage(EntityPlayer player) {
        sendMessage(player, "§cИспользование: /auth admin add <игрок>");
    }


    @Override
    public boolean canCommandSenderUseCommand(ICommandSender sender) {
        return true;
    }

}