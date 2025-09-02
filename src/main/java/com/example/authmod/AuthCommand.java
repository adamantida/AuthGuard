package com.example.authmod;

import net.minecraft.command.CommandBase;
import net.minecraft.command.ICommandSender;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.event.ClickEvent;
import net.minecraft.event.HoverEvent;
import net.minecraft.util.ChatComponentText;
import net.minecraft.util.ChatStyle;
import net.minecraft.util.EnumChatFormatting;
import net.minecraft.util.IChatComponent;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Команда аутентификации для регистрации и входа игроков.
 * Предоставляет безопасный механизм авторизации на сервере.
 */
public class AuthCommand extends CommandBase {

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

    private void sendUsageMessage(EntityPlayer player) {
        sendMessage(player, "§cИспользование: /auth <register|login|logout|changepassword|admin>");
        sendMessage(player, "§eКраткие команды: /a r, /a l, /a out");
    }

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
                int page = 1;
                if (args.length > 1) {
                    filter = args[1].toLowerCase();
                }
                if (args.length > 2) {
                    try {
                        page = Integer.parseInt(args[2]);
                    } catch (NumberFormatException e) {
                        page = 1;
                    }
                }
                handleAdminList(player, filter, page);
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
        sendMessage(player, String.format("§eДата регистрации: %s", data.getRegistrationDate()));
        sendMessage(player, String.format("§eПоследний вход: %s", data.getLastLoginDate()));
        sendMessage(player, String.format("§eСтатус: %s", banStatus));
    }

    private void handleAdminList(EntityPlayer player, String filter, int page) {
        List<PlayerData> players = PlayerDataManager.getAllPlayers();
        players = applyFilter(players, filter);

        if (players.isEmpty()) {
            sendMessage(player, "§cНет игроков, соответствующих фильтру: " + getFilterDisplayName(filter));
            return;
        }

        final int pageSize = 10;
        int totalPages = (players.size() + pageSize - 1) / pageSize;
        if (page < 1) page = 1;
        if (page > totalPages) page = totalPages;
        int start = (page - 1) * pageSize;
        int end = Math.min(start + pageSize, players.size());

        List<PlayerData> pageData = players.subList(start, end);

        // Отправляем заголовок с фильтрами
        sendAdminListHeader(player, filter, page, totalPages);

        // Отправляем заголовок таблицы
        sendMessage(player, "§8────────────────────────────────────────────────");
        sendMessage(player, "§7Игрок         §8| §7Последний IP      §8| §7Рег. IP        §8| §7Статус");
        sendMessage(player, "§8────────────────────────────────────────────────");

        // Форматируем и выводим данные игроков
        for (PlayerData data : pageData) {
            String banStatus = data.isBanned() ? "§cБан" : "§aАктив";

            // Форматируем каждую колонку с учетом цветовых кодов
            String username = formatWithColor("§f" + data.getUsername(), 14);
            String lastIP = formatWithColor("§b" + data.getLastLoginIP(), 16);
            String regIP = formatWithColor("§3" + data.getRegistrationIP(), 16);

            String message = String.format("%s §8| %s §8| %s §8| %s", username, lastIP, regIP, banStatus);
            sendMessage(player, message);
        }

        sendMessage(player, "§8────────────────────────────────────────────────");

        // Отправляем пагинацию и статистику
        sendMessage(player, String.format("§6Страница %d/%d §8│ §7Найдено: §f%d §8│ §7Фильтр: %s",
                page, totalPages, players.size(), getFilterDisplayName(filter)));
        sendPaginationControls(player, page, totalPages, filter);
    }

    private String formatWithColor(String text, int width) {
        // Удаляем цветовые коды для расчета видимой длины
        String cleanText = stripColorCodes(text);

        // Обрезаем текст до нужной длины
        if (cleanText.length() > width) {
            cleanText = cleanText.substring(0, width);

            // Восстанавливаем цветовые коды в обрезанном тексте
            StringBuilder result = new StringBuilder();
            boolean isColorCode = false;
            for (int i = 0; i < text.length() && result.length() < cleanText.length(); i++) {
                char c = text.charAt(i);
                if (c == '§') {
                    isColorCode = true;
                    result.append(c);
                } else if (isColorCode) {
                    isColorCode = false;
                    result.append(c);
                } else {
                    result.append(c);
                }
            }
            text = result.toString();
        }

        int visibleLength = cleanText.length();
        int padding = width - visibleLength;
        if (padding > 0) {
            StringBuilder spaces = new StringBuilder(text);
            for (int i = 0; i < padding; i++) {
                spaces.append(' ');
            }
            return spaces.toString();
        }
        return text;
    }

    private String stripColorCodes(String input) {
        return input.replaceAll("§[0-9a-fk-or]", "");
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
        IChatComponent controls = new ChatComponentText("");

        // Блок навигации
        IChatComponent navBlock = new ChatComponentText("§6« ");
        navBlock.setChatStyle(new ChatStyle()
                .setColor(EnumChatFormatting.DARK_GRAY)
                .setChatClickEvent(new ClickEvent(ClickEvent.Action.RUN_COMMAND,
                        String.format("/auth admin list %s 1", filter)))
                .setUnderlined(currentPage > 1));
        controls.appendSibling(navBlock);

        // Кнопка "Назад"
        if (currentPage > 1) {
            ChatComponentText prevBtn = new ChatComponentText("← ");
            prevBtn.setChatStyle(new ChatStyle()
                    .setColor(EnumChatFormatting.YELLOW)
                    .setChatClickEvent(new ClickEvent(ClickEvent.Action.RUN_COMMAND,
                            String.format("/auth admin list %s %d", filter, currentPage - 1))));
            controls.appendSibling(prevBtn);
        }

        // Информация о странице
        ChatComponentText pageInfo = new ChatComponentText(String.format("§e%d§7/§e%d ", currentPage, totalPages));
        pageInfo.setChatStyle(new ChatStyle().setBold(true));
        controls.appendSibling(pageInfo);

        // Кнопка "Вперёд"
        if (currentPage < totalPages) {
            ChatComponentText nextBtn = new ChatComponentText("→");
            nextBtn.setChatStyle(new ChatStyle()
                    .setColor(EnumChatFormatting.YELLOW)
                    .setChatClickEvent(new ClickEvent(ClickEvent.Action.RUN_COMMAND,
                            String.format("/auth admin list %s %d", filter, currentPage + 1))));
            controls.appendSibling(nextBtn);
        }

        // Блок конца
        IChatComponent endBlock = new ChatComponentText(" §6»");
        endBlock.setChatStyle(new ChatStyle()
                .setColor(EnumChatFormatting.DARK_GRAY)
                .setChatClickEvent(new ClickEvent(ClickEvent.Action.RUN_COMMAND,
                        String.format("/auth admin list %s %d", filter, totalPages)))
                .setUnderlined(currentPage < totalPages));
        controls.appendSibling(endBlock);

        // Статистика
        ChatComponentText stats = new ChatComponentText(String.format(" §8| §7Найдено: §f%d", PlayerDataManager.getAllPlayers().size()));
        controls.appendSibling(stats);

        player.addChatMessage(controls);
    }

    private void sendAdminListHeader(EntityPlayer player, String filter, int currentPage, int totalPages) {
        IChatComponent header = new ChatComponentText("§6[Фильтры]: ");

        // Создаем кнопки с разделителями
        List<ChatComponentText> filterButtons = new ArrayList<>();

        // Генерируем кнопки с учетом текущего фильтра
        filterButtons.add(createFilterButton("Все", "all", filter));
        filterButtons.add(createFilterButton("Бан", "banned", filter));
        filterButtons.add(createFilterButton("5м", "5min", filter));
        filterButtons.add(createFilterButton("15м", "15min", filter));
        filterButtons.add(createFilterButton("30м", "30min", filter));
        filterButtons.add(createFilterButton("60м", "60min", filter));

        // Добавляем разделители
        for (int i = 0; i < filterButtons.size(); i++) {
            if (i > 0) {
                header.appendSibling(new ChatComponentText("§7, "));
            }
            header.appendSibling(filterButtons.get(i));
        }
        player.addChatMessage(header);
    }

    private ChatComponentText createFilterButton(String name, String filterValue, String currentFilter) {
        String icon = "";
        switch (filterValue) {
            case "all": icon = "👤"; break;
            case "banned": icon = "⛔"; break;
            case "5min": icon = "🟢"; break;
            case "15min": icon = "🟡"; break;
            case "30min": icon = "🟠"; break;
            case "60min": icon = "🔴"; break;
        }

        // Создаем текст кнопки с иконкой
        ChatComponentText btn = new ChatComponentText(String.format("%s [%s]", icon, name));
        ChatStyle style = new ChatStyle();

        // Выделяем текущий фильтр жирным шрифтом и другим цветом [[4]]
        if (filterValue.equals(currentFilter)) {
            style.setColor(EnumChatFormatting.YELLOW);
            style.setBold(true);
            style.setUnderlined(true);
        } else {
            // Цвета для неактивных фильтров
            switch (filterValue) {
                case "all":
                    style.setColor(EnumChatFormatting.GREEN);
                    break;
                case "banned":
                    style.setColor(EnumChatFormatting.RED);
                    break;
                default:
                    style.setColor(EnumChatFormatting.GRAY);
                    break;
            }
        }

        // Настраиваем клик для изменения фильтра
        btn.setChatStyle(style.setChatClickEvent(new ClickEvent(
                ClickEvent.Action.RUN_COMMAND,
                "/auth admin list " + filterValue
        )));

        return btn;
    }

    private String getFilterDisplayName(String filter) {
        switch (filter) {
            case "all": return "Все игроки";
            case "banned": return "В бане";
            case "5min": return "Активные (5м)";
            case "15min": return "Активные (15м)";
            case "30min": return "Активные (30м)";
            case "60min": return "Активные (60м)";
            default: return filter;
        }
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