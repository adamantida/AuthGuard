// Файл: BanStatusSyncer.java
package com.example.authmod;

import cpw.mods.fml.common.FMLCommonHandler;
import cpw.mods.fml.common.eventhandler.SubscribeEvent;
import cpw.mods.fml.common.gameevent.TickEvent;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.management.ServerConfigurationManager;

import java.lang.reflect.Method;
import java.util.List;
import java.util.Set;

/**
 * Синхронизатор статуса бана игроков.
 * Периодически проверяет статус бана через сервер и обновляет данные.
 * Использует рефлексию для совместимости с API 1.7.10.
 */
public class BanStatusSyncer {
    private static final int SYNC_INTERVAL = 20 * 5; // Каждые 5 секунд
    private int ticks = 0;

    @SubscribeEvent
    public void onServerTick(TickEvent.ServerTickEvent event) {
        if (event.phase != TickEvent.Phase.START) {
            return;
        }
        ticks++;
        if (ticks >= SYNC_INTERVAL) {
            ticks = 0;
            syncBanStatus();
        }
    }

    private void syncBanStatus() {
        MinecraftServer server = FMLCommonHandler.instance().getMinecraftServerInstance();
        if (server == null) {
            return;
        }

        ServerConfigurationManager configManager = server.getConfigurationManager();
        if (configManager == null) {
            AuthMod.logger.warn("ServerConfigurationManager is null, cannot sync ban status.");
            return;
        }

        try {
            // В 1.7.10 getBannedPlayers() возвращает BanList (или похожий внутренний класс)
            // Используем рефлексию для вызова метода
            Method getBannedPlayersMethod = ServerConfigurationManager.class.getDeclaredMethod("func_72393_m"); // SRG имя для getBannedPlayers
            getBannedPlayersMethod.setAccessible(true); // Делаем метод доступным, если он приватный
            Object banListObject = getBannedPlayersMethod.invoke(configManager);

            if (banListObject == null) {
                AuthMod.logger.warn("getBannedPlayers() returned null.");
                return;
            }

            // Получаем список имен забаненных игроков
            // В 1.7.10 BanList имеет метод func_73710_b (getBannedList) или используем поле напрямую через рефлексию
            // Проще получить Set или List напрямую, если возможно, или использовать рефлексию для isBanned

            // Альтернативный подход: получить список имен напрямую через рефлексию
            // Ищем поле, содержащее карту или список забаненных
            // Это может быть поле типа Map или Set, содержащее имена или объекты BanEntry
            // Для простоты, попробуем получить метод isBanned для каждого игрока

            // Получаем список всех зарегистрированных игроков
            List<String> allPlayerNames = PlayerDataManager.getAllPlayerNames();
            for (String username : allPlayerNames) {
                // Используем рефлексию для проверки бана
                // В 1.7.10 BanList.isBanned(Object) или isBanned(String)
                Method isBannedMethod = banListObject.getClass().getDeclaredMethod("func_73706_a", Object.class); // SRG имя для isBanned
                isBannedMethod.setAccessible(true); // Делаем метод доступным
                // Вызываем isBanned с именем пользователя
                Object isBannedResult = isBannedMethod.invoke(banListObject, username);
                boolean isBanned = false;
                if (isBannedResult instanceof Boolean) {
                    isBanned = (Boolean) isBannedResult;
                } else {
                    AuthMod.logger.warn("isBanned method returned unexpected type: " + (isBannedResult != null ? isBannedResult.getClass().getName() : "null"));
                    // Предположим, не забанен, если результат не булев
                }
                PlayerDataManager.setPlayerBanned(username, isBanned);
            }

        } catch (Exception e) {
            // Если SRG имя не сработало, попробуем декомпилированное имя
            try {
                AuthMod.logger.debug("Trying deobfuscated method names for ban list...");
                Method getBannedPlayersMethod = ServerConfigurationManager.class.getDeclaredMethod("getBannedPlayers");
                getBannedPlayersMethod.setAccessible(true);
                Object banListObject = getBannedPlayersMethod.invoke(configManager);

                if (banListObject == null) {
                    AuthMod.logger.warn("getBannedPlayers() returned null (deobf).");
                    return;
                }

                List<String> allPlayerNames = PlayerDataManager.getAllPlayerNames();
                for (String username : allPlayerNames) {
                    Method isBannedMethod = banListObject.getClass().getDeclaredMethod("isBanned", Object.class);
                    isBannedMethod.setAccessible(true);
                    Object isBannedResult = isBannedMethod.invoke(banListObject, username);
                    boolean isBanned = false;
                    if (isBannedResult instanceof Boolean) {
                        isBanned = (Boolean) isBannedResult;
                    }
                    PlayerDataManager.setPlayerBanned(username, isBanned);
                }

            } catch (Exception e2) {
                AuthMod.logger.error("Failed to sync ban status using reflection. Check Minecraft/Forge version compatibility.", e2);
                // Сбрасываем статус бана на false для всех, если не удалось получить информацию
                List<String> allPlayerNames = PlayerDataManager.getAllPlayerNames();
                for (String username : allPlayerNames) {
                    PlayerDataManager.setPlayerBanned(username, false);
                }
            }
        }
    }
}