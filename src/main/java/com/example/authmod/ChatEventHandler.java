package com.example.authmod;

import cpw.mods.fml.common.eventhandler.SubscribeEvent;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.ChatComponentText;
import net.minecraftforge.event.ServerChatEvent;

/**
 * Обработчик событий чата.
 * Контролирует отправку сообщений неавторизованными игроками.
 */
public class ChatEventHandler {

    /** Интервал отправки сообщений (раз в 5 секунд) */
    private static final int MESSAGE_INTERVAL = 100;

    // ========================================================================
    // Обработчики событий
    // ========================================================================

    @SubscribeEvent
    public void onChat(ServerChatEvent event) {
        EntityPlayer player = event.player;
        if (player == null) return;

        String message = event.message;
        String username = AuthEventHandler.normalizeUsername(player.getCommandSenderName());

        // Обновляем время активности (раз в 10 сообщений)
        if (player.ticksExisted % 10 == 0) {
            AuthEventHandler.updateLoginTime(username);
        }

        // Проверяем, является ли сообщение командой
        if (message.startsWith("/")) {
            handleCommandMessage(event, player);
        }
        // Обычное сообщение в чат
        else {
            handleChatMessage(event, player);
        }
    }

    // ========================================================================
    // Внутренние методы обработки
    // ========================================================================

    private void handleCommandMessage(ServerChatEvent event, EntityPlayer player) {
        // Проверяем только разрешенные команды
        if (!AuthEventHandler.isPlayerAuthenticated(player) && !isAuthCommand(event.message)) {
            event.setCanceled(true);
            sendAuthRequiredMessage(player);
        }
    }

    private void handleChatMessage(ServerChatEvent event, EntityPlayer player) {
        if (!AuthEventHandler.isPlayerAuthenticated(player)) {
            event.setCanceled(true);
            sendAuthRequiredMessage(player);
        }
    }

    private boolean isAuthCommand(String message) {
        String command = message.substring(1).split(" ")[0].toLowerCase();
        return "auth".equals(command) || "a".equals(command);
    }

    private void sendAuthRequiredMessage(EntityPlayer player) {
        // Отправляем сообщение только раз в 5 секунд
        if (player.ticksExisted % MESSAGE_INTERVAL == 0) {
            try {
                player.addChatMessage(new ChatComponentText("§cВы должны авторизоваться перед использованием чата!"));
            } catch (Exception e) {
                // Игнорируем ошибки отправки сообщений
            }
        }
    }
}