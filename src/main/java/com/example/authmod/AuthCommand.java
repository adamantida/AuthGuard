package com.example.authmod;

import net.minecraft.command.CommandBase;
import net.minecraft.command.ICommandSender;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.ChatComponentText;

import java.util.Arrays;
import java.util.List;

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
        return "/auth <register|login> <пароль> [подтверждение]";
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
        if (args.length < 2) {
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
        sendMessage(player, "§cИспользование: /auth <register|login> <пароль> [подтверждение]");
    }

    /**
     * Обрабатывает действие регистрации или входа
     */
    private void processAuthAction(EntityPlayer player, String username, String[] args) {
        String action = args[0].toLowerCase();
        String password = args[1];

        try {
            switch (action) {
                case "register":
                    handleRegistration(player, username, args);
                    break;
                case "login":
                    handleLogin(player, username, password);
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
    public List addTabCompletionOptions(ICommandSender sender, String[] args) {
        // Предлагаем автодополнение только для первого аргумента
        if (args.length == 1) {
            return getListOfStringsMatchingLastWord(args, "register", "login");
        }
        return null;
    }

    // ========================================================================
    // Обработчики действий
    // ========================================================================

    private void handleRegistration(EntityPlayer player, String username, String[] args) {
        // Проверка наличия подтверждения пароля
        if (args.length < 3) {
            sendPasswordConfirmationRequired(player);
            return;
        }

        // Проверка, что игрок еще не зарегистрирован
        if (PlayerDataManager.isPlayerRegistered(username)) {
            sendAlreadyRegisteredMessage(player);
            return;
        }

        // Проверка совпадения паролей
        if (!doPasswordsMatch(args[1], args[2])) {
            sendPasswordsDoNotMatch(player);
            return;
        }

        // Проверка длины пароля
        if (isPasswordTooShort(args[1])) {
            sendPasswordTooShort(player);
            return;
        }

        // Регистрация игрока
        registerPlayer(player, username, args[1]);
    }

    private void handleLogin(EntityPlayer player, String username, String password) {
        // Проверка, что игрок зарегистрирован
        if (!PlayerDataManager.isPlayerRegistered(username)) {
            sendNotRegisteredMessage(player);
            return;
        }

        // Проверка пароля
        if (AuthHelper.verifyPassword(password, PlayerDataManager.getPlayerHash(username))) {
            AuthEventHandler.authenticatePlayer(player);
            sendLoginSuccessMessage(player);
        } else {
            sendIncorrectPasswordMessage(player);
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
        PlayerDataManager.registerPlayer(username, password);
        AuthEventHandler.authenticatePlayer(player);
        sendRegistrationSuccessMessage(player);
    }

    private void sendUnknownCommandMessage(EntityPlayer player) {
        sendMessage(player, "§cНеизвестная команда! Используйте register или login");
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