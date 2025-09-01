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

    /** Допустимые алиасы команды */
    private static final List<String> ALIASES = Arrays.asList("auth", "a");

    // ========================================================================
    // Основные методы команды
    // ========================================================================

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

    // ========================================================================
    // Обработка команд
    // ========================================================================

    @Override
    public void processCommand(ICommandSender sender, String[] args) {
        // Проверка, что отправитель - игрок
        if (!(sender instanceof EntityPlayer)) {
            return;
        }

        EntityPlayer player = (EntityPlayer) sender;
        String username = AuthEventHandler.normalizeUsername(player.getCommandSenderName());

        // Сброс таймера активности
        AuthEventHandler.updateLoginTime(username);

        // Проверка аргументов
        if (args.length < 1) {
            sendUsageMessage(player);
            return;
        }

        // Обработка действия
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
        String action = args[0].toLowerCase();
        String[] newArgs = args;

        // Обработка коротких алиасов
        if (action.equals("l")) {
            action = "login";
            newArgs = Arrays.copyOfRange(args, 1, args.length);
        } else if (action.equals("r")) {
            action = "register";
            newArgs = Arrays.copyOfRange(args, 1, args.length);
        } else if (action.equals("out")) {
            action = "logout";
            newArgs = Arrays.copyOfRange(args, 1, args.length);
        }

        try {
            switch (action) {
                case "register":
                    handleRegistration(player, username, newArgs);
                    break;
                case "login":
                    handleLogin(player, username, newArgs);
                    break;
                case "logout":
                    handleLogout(player, username);
                    break;
                case "changepassword":
                    handleChangePassword(player, username, newArgs);
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

    // ========================================================================
    // Обработка автодополнения Tab
    // ========================================================================
    @Override
    public List<String> addTabCompletionOptions(ICommandSender sender, String[] args) {
        // Предлагаем автодополнение только для первого аргумента
        if (args.length == 1) {
            return getListOfStringsMatchingLastWord(args, "register", "login", "logout", "changepassword", "admin", "r", "l", "out");
        } else if (args.length == 2 && args[0].equalsIgnoreCase("admin")) {
            return getListOfStringsMatchingLastWord(args, "reset", "list", "ip");
        } else if (args.length == 3 && args[0].equalsIgnoreCase("admin") && args[1].equalsIgnoreCase("list")) {
            return getListOfStringsMatchingLastWord(args, "all", "banned", "5min", "15min", "30min", "60min");
        }
        return null;
    }

    // ========================================================================
    // Обработчики действий
    // ========================================================================

    private void handleRegistration(EntityPlayer player, String username, String[] args) {
        // Проверка наличия подтверждения пароля
        if (args.length < 2) {
            sendPasswordConfirmationRequired(player);
            return;
        }

        // Проверка, что игрок еще не зарегистрирован
        if (PlayerDataManager.isPlayerRegistered(username)) {
            sendAlreadyRegisteredMessage(player);
            return;
        }

        // Проверка совпадения паролей
        if (!doPasswordsMatch(args[0], args[1])) {
            sendPasswordsDoNotMatch(player);
            return;
        }

        // Проверка длины пароля
        if (isPasswordTooShort(args[0])) {
            sendPasswordTooShort(player);
            return;
        }

        // Регистрация игрока
        registerPlayer(player, username, args[0]);
    }

    private void handleLogin(EntityPlayer player, String username, String[] args) {
        if (args.length < 1) {
            sendIncorrectPasswordMessage(player);
            return;
        }

        String password = args[0];

        // Проверка, что игрок зарегистрирован
        if (!PlayerDataManager.isPlayerRegistered(username)) {
            sendNotRegisteredMessage(player);
            return;
        }

        // Проверка пароля
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

        // Используем публичный метод вместо прямого доступа к приватным полям
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

        // Проверка старого пароля
        if (!AuthHelper.verifyPassword(oldPassword, PlayerDataManager.getPlayerHash(username))) {
            sendIncorrectOldPasswordMessage(player);
            return;
        }

        // Проверка нового пароля
        if (!newPassword.equals(confirmPassword)) {
            sendPasswordsDoNotMatch(player);
            return;
        }

        if (isPasswordTooShort(newPassword)) {
            sendPasswordTooShort(player);
            return;
        }

        // Обновление пароля через публичный метод
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
        // В 1.7.10 используется canCommandSenderUseCommand вместо canUseCommand
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
                handleAdminList(player, args.length > 1 ? args[1] : "all");
                break;
            case "ip":
                if (args.length < 2) {
                    sendAdminIPUsage(player);
                    return;
                }
                handleAdminIP(player, args[1]);
                break;
            default:
                sendAdminUsageMessage(player);
        }
    }

    // ========================================================================
    // Вспомогательные методы
    // ========================================================================

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
        sendMessage(player, "§e/auth admin list [all|banned|5min|15min|30min|60min] §7- список игроков");
        sendMessage(player, "§e/auth admin ip <игрок> §7- информация об IP");
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
        String message = String.format("§6Информация об игроке %s:\n" +
                        "§eПоследний IP: %s\n" +
                        "§eIP регистрации: %s\n" +
                        "§eСтатус: %s",
                username, data.getLastLoginIP(), data.getRegistrationIP(), banStatus);

        sendMessage(player, message);
    }

    private void handleAdminList(EntityPlayer player, String filter) {
        List<PlayerData> players = PlayerDataManager.getAllPlayers();

        // Применяем фильтр
        long currentTime = System.currentTimeMillis();
        long fiveMinutesAgo = currentTime - 5 * 60 * 1000;
        long fifteenMinutesAgo = currentTime - 15 * 60 * 1000;
        long thirtyMinutesAgo = currentTime - 30 * 60 * 1000;
        long sixtyMinutesAgo = currentTime - 60 * 60 * 1000;

        switch (filter.toLowerCase()) {
            case "banned":
                players = players.stream()
                        .filter(PlayerData::isBanned)
                        .collect(Collectors.toList());
                break;
            case "5min":
                players = players.stream()
                        .filter(p -> p.getRegistrationDate() >= fiveMinutesAgo)
                        .collect(Collectors.toList());
                break;
            case "15min":
                players = players.stream()
                        .filter(p -> p.getRegistrationDate() >= fifteenMinutesAgo)
                        .collect(Collectors.toList());
                break;
            case "30min":
                players = players.stream()
                        .filter(p -> p.getRegistrationDate() >= thirtyMinutesAgo)
                        .collect(Collectors.toList());
                break;
            case "60min":
                players = players.stream()
                        .filter(p -> p.getRegistrationDate() >= sixtyMinutesAgo)
                        .collect(Collectors.toList());
                break;
            default:
                // Все игроки (без фильтрации)
                break;
        }

        if (players.isEmpty()) {
            sendMessage(player, "§cНет игроков, соответствующих фильтру: " + filter);
            return;
        }

        // Отправка заголовка с интерактивными кнопками
        sendAdminListHeader(player);

        // Отправка данных игроков
        for (PlayerData data : players) {
            String banStatus = data.isBanned() ? "§cЗАБАНЕН" : "§aАКТИВЕН";
            String message = String.format("§e%s §7| §bПосл. IP: %s §7| §bРег. IP: %s §7| §eСтатус: %s",
                    data.getUsername(), data.getLastLoginIP(), data.getRegistrationIP(), banStatus);

            sendMessage(player, message);
        }

        sendMessage(player, String.format("§6Найдено игроков: §e%d", players.size()));
    }

    private void sendAdminListHeader(EntityPlayer player) {
        // Создаем интерактивные кнопки для фильтрации
        IChatComponent header = new ChatComponentText("§6Список игроков | Фильтры: ");

        // Кнопка "Все"
        ChatComponentText allBtn = new ChatComponentText("[Все] ");
        allBtn.setChatStyle(new ChatStyle()
                .setColor(EnumChatFormatting.GREEN)
                .setChatClickEvent(new ClickEvent(ClickEvent.Action.RUN_COMMAND, "/auth admin list all")));

        // Кнопка "Забаненные"
        ChatComponentText bannedBtn = new ChatComponentText("[Забаненные] ");
        bannedBtn.setChatStyle(new ChatStyle()
                .setColor(EnumChatFormatting.RED)
                .setChatClickEvent(new ClickEvent(ClickEvent.Action.RUN_COMMAND, "/auth admin list banned")));

        // Кнопки времени
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

        // Добавляем все компоненты к заголовку
        header.appendSibling(allBtn);
        header.appendSibling(bannedBtn);
        header.appendSibling(last5min);
        header.appendSibling(last15min);
        header.appendSibling(last30min);
        header.appendSibling(last60min);

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

    @Override
    public boolean canCommandSenderUseCommand(ICommandSender sender) {
        return true;
    }

}