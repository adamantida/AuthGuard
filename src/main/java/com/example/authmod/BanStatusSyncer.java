
package com.example.authmod;

import cpw.mods.fml.common.FMLCommonHandler;
import cpw.mods.fml.common.eventhandler.SubscribeEvent;
import cpw.mods.fml.common.gameevent.TickEvent;
import net.minecraft.server.MinecraftServer;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.io.File;
import java.io.FileReader;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Синхронизатор статуса бана игроков.
 * Периодически проверяет статус бана, читая файл banned-players.json.
 */
public class BanStatusSyncer {
    private static final int SYNC_INTERVAL = 20 * 30; // Каждые 30 секунд (можно настроить)
    private int ticks = 0;
    private long lastModifiedTime = -1; // Время последней модификации файла
    private Set<String> lastBannedUsernames = new HashSet<>(); // Кэш последнего прочитанного списка

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
            AuthMod.logger.warn("[BanStatusSyncer] MinecraftServer instance is null.");
            return;
        }

        File serverDir = server.getFile(""); // Получает корневую директорию сервера
        if (serverDir == null || !serverDir.exists()) {
            AuthMod.logger.error("[BanStatusSyncer] Could not determine server directory.");
            resetBanStatus(); // Сбросить статус бана
            return;
        }

        File bannedPlayersFile = new File(serverDir, "banned-players.json");
        if (!bannedPlayersFile.exists()) {
            AuthMod.logger.warn("[BanStatusSyncer] banned-players.json file does not exist. Assuming no players are banned.");
            resetBanStatus();
            return;
        }

        long currentModifiedTime = bannedPlayersFile.lastModified();

        if (currentModifiedTime != lastModifiedTime) {
            AuthMod.logger.debug("[BanStatusSyncer] banned-players.json has changed or is read for the first time. Reloading...");
            Set<String> currentBannedUsernames = new HashSet<>();

            try (FileReader reader = new FileReader(bannedPlayersFile)) {
                JsonElement rootElement = new JsonParser().parse(reader);

                if (rootElement.isJsonArray()) {

                    JsonArray banArray = rootElement.getAsJsonArray();
                    for (JsonElement element : banArray) {
                        if (element.isJsonObject()) {
                            JsonObject banEntry = element.getAsJsonObject();
                            JsonElement nameElement = banEntry.get("name");
                            if (nameElement != null && nameElement.isJsonPrimitive()) {

                                currentBannedUsernames.add(nameElement.getAsString());
                            }
                        }
                    }
                } else if (rootElement.isJsonObject()) {

                    JsonObject banObject = rootElement.getAsJsonObject();
                    for (String username : banObject.keySet()) {

                        currentBannedUsernames.add(username);
                    }
                } else {
                    AuthMod.logger.warn("[BanStatusSyncer] banned-players.json root element is neither an Object nor an Array. Assuming no players are banned.");
                }

                lastBannedUsernames = currentBannedUsernames;
                lastModifiedTime = currentModifiedTime;
                AuthMod.logger.info("[BanStatusSyncer] Successfully loaded {} banned players from file.", currentBannedUsernames.size());

            } catch (Exception e) {
                AuthMod.logger.error("[BanStatusSyncer] Failed to read or parse banned-players.json", e);
                resetBanStatus(); // Сбросить статус бана в случае ошибки чтения/парсинга
                return; // Не продолжаем обработку, если не удалось прочитать
            }
        }


        List<String> allPlayerNames = PlayerDataManager.getAllPlayerNames();
        AuthMod.logger.debug("[BanStatusSyncer] Checking ban status for {} registered players against {} loaded bans.", allPlayerNames.size(), lastBannedUsernames.size());

        for (String username : allPlayerNames) {

            boolean isBanned = lastBannedUsernames.contains(username);

            PlayerDataManager.setPlayerBanned(username, isBanned);
        }
    }

    /**
     * Сбрасывает статус бана для всех игроков на false.
     * Вызывается в случае ошибки или если файл не существует.
     */
    private void resetBanStatus() {
        AuthMod.logger.warn("[BanStatusSyncer] Resetting ban status for all players to false.");
        List<String> allPlayerNames = PlayerDataManager.getAllPlayerNames();
        for (String username : allPlayerNames) {
            PlayerDataManager.setPlayerBanned(username, false);
        }

        lastBannedUsernames.clear();
        lastModifiedTime = -1;
    }
}