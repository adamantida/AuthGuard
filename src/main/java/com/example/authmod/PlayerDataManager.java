package com.example.authmod;

import com.google.gson.*;

import java.io.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Менеджер данных игроков.
 * Отвечает за сохранение и загрузку данных об авторизации.
 */
public class PlayerDataManager {
    private static final Gson GSON = new GsonBuilder().setPrettyPrinting().create();

    private static final Map<String, PlayerData> PLAYER_DATA_MAP = new ConcurrentHashMap<>();

    private static final int SAVE_DELAY = 20 * 10;

    private static final AtomicBoolean savePending = new AtomicBoolean(false);

    private static final ScheduledExecutorService SAVE_EXECUTOR = Executors.newSingleThreadScheduledExecutor(r -> {
                Thread t = new Thread(r);
                t.setName("Auth-Save-Worker");
                t.setDaemon(true);
                return t;
            });

    private static File DATA_FILE;

    public static void init(File configDir) {
        DATA_FILE = new File(configDir, "authdata.json");
        loadData();
    }

    public static boolean isPlayerRegistered(String username) {
        return PLAYER_DATA_MAP.containsKey(username);
    }

    public static String getPlayerHash(String username) {
        PlayerData data = PLAYER_DATA_MAP.get(username);
        return (data != null) ? data.getHashedPassword() : null;
    }

    public static void registerPlayer(String username, String password, String ip) {
        String hashedPassword = AuthHelper.hashPassword(password);
        long now = System.currentTimeMillis();
        PlayerData data = new PlayerData(username, hashedPassword, now, now,
                ip, ip, false, false);
        PLAYER_DATA_MAP.put(username, data);
        scheduleSave();
    }

    public static void updateLoginData(String username, String ip) {
        PlayerData oldData = PLAYER_DATA_MAP.get(username);
        long now = System.currentTimeMillis();
        if (oldData != null) {
            PlayerData newData = new PlayerData(
                    oldData.getUsername(),
                    oldData.getHashedPassword(),
                    oldData.getRegistrationDate(),
                    now,
                    oldData.getRegistrationIP(),
                    ip,
                    oldData.isBanned(),
                    oldData.isOperator()
            );
            PLAYER_DATA_MAP.put(username, newData);
            scheduleSave();
        }
    }

    public static PlayerData getPlayerData(String username) {
        return PLAYER_DATA_MAP.get(username);
    }

    public static boolean resetPlayerPassword(String username) {
        PlayerData oldData = PLAYER_DATA_MAP.get(username);
        if (oldData != null) {

            String tempPassword = "12345678";
            String hashedPassword = AuthHelper.hashPassword(tempPassword);

            PlayerData newData = new PlayerData(
                    oldData.getUsername(),
                    hashedPassword,
                    oldData.getRegistrationDate(),
                    oldData.getLastLoginDate(),
                    oldData.getRegistrationIP(),
                    oldData.getLastLoginIP(),
                    oldData.isBanned(),
                    oldData.isOperator()
            );

            PLAYER_DATA_MAP.put(username, newData);
            scheduleSave();
            return true;
        }
        return false;
    }

    public static boolean updatePassword(String username, String newPassword) {
        PlayerData oldData = PLAYER_DATA_MAP.get(username);
        if (oldData != null) {
            String hashedPassword = AuthHelper.hashPassword(newPassword);
            PlayerData newData = new PlayerData(
                    oldData.getUsername(),
                    hashedPassword,
                    oldData.getRegistrationDate(),
                    oldData.getLastLoginDate(),
                    oldData.getRegistrationIP(),
                    oldData.getLastLoginIP(),
                    oldData.isBanned(),
                    oldData.isOperator()
            );
            PLAYER_DATA_MAP.put(username, newData);
            scheduleSave();
            return true;
        }
        return false;
    }

    public static void setPlayerBanned(String username, boolean banned) {
        PlayerData oldData = PLAYER_DATA_MAP.get(username);
        if (oldData != null) {
            PlayerData newData = new PlayerData(
                    oldData.getUsername(),
                    oldData.getHashedPassword(),
                    oldData.getRegistrationDate(),
                    oldData.getLastLoginDate(),
                    oldData.getRegistrationIP(),
                    oldData.getLastLoginIP(),
                    banned,
                    oldData.isOperator()
            );
            PLAYER_DATA_MAP.put(username, newData);
            scheduleSave();
        }
    }

    public static List<PlayerData> getAllPlayers() {
        return new ArrayList<>(PLAYER_DATA_MAP.values());
    }

    private static void loadData() {
        if (DATA_FILE.exists()) {
            try (Reader reader = new FileReader(DATA_FILE)) {

                JsonObject json = new JsonParser().parse(reader).getAsJsonObject();
                Map<String, PlayerData> loadedData = new HashMap<>();

                for (Map.Entry<String, JsonElement> entry : json.entrySet()) {
                    String username = entry.getKey();
                    JsonObject playerJson = entry.getValue().getAsJsonObject();

                    String registrationIP = "unknown";
                    String lastLoginIP = "unknown";
                    long registrationDate = 0;
                    long lastLoginDate = 0;
                    boolean isBanned = false;
                    boolean isOperator = false;

                    if (playerJson.has("registrationDate")) {
                        registrationDate = playerJson.get("registrationDate").getAsLong();
                    }
                    if (playerJson.has("lastLoginDate")) {
                        lastLoginDate = playerJson.get("lastLoginDate").getAsLong();
                    } else {
                        lastLoginDate = registrationDate;
                    }

                    if (playerJson.has("registrationIP")) {
                        registrationIP = playerJson.get("registrationIP").getAsString();
                    }

                    if (playerJson.has("lastLoginIP")) {
                        lastLoginIP = playerJson.get("lastLoginIP").getAsString();
                    }

                    if (playerJson.has("isBanned")) {
                        isBanned = playerJson.get("isBanned").getAsBoolean();
                    }

                    if (playerJson.has("isOperator")) {
                        isOperator = playerJson.get("isOperator").getAsBoolean();
                    }

                    PlayerData data = new PlayerData(
                            username,
                            playerJson.get("hashedPassword").getAsString(),
                            registrationDate,
                            lastLoginDate,
                            registrationIP,
                            lastLoginIP,
                            isBanned,
                            isOperator
                    );

                    loadedData.put(username, data);
                }

                if (!loadedData.isEmpty()) {
                    PLAYER_DATA_MAP.clear();
                    PLAYER_DATA_MAP.putAll(loadedData);
                }
            } catch (Exception e) {
                AuthMod.logger.error("Failed to load player data", e);

                PLAYER_DATA_MAP.clear();
            }
        }
    }

    private static void scheduleSave() {
        if (savePending.getAndSet(true)) {
            return;
        }

        SAVE_EXECUTOR.schedule(() -> {
            savePending.set(false);
            saveData();
        }, SAVE_DELAY, TimeUnit.MILLISECONDS);
    }

    private static void saveData() {
        try (Writer writer = new FileWriter(DATA_FILE)) {
            GSON.toJson(PLAYER_DATA_MAP, writer);
        } catch (IOException e) {
            AuthMod.logger.error("Failed to save player data", e);
        }
    }

    public static List<String> getAllPlayerNames() {
        return new ArrayList<>(PLAYER_DATA_MAP.keySet());
    }

    public static boolean isPlayerOperator(String username) {

        String normalizedUsername = AuthEventHandler.normalizeUsername(username);
        PlayerData data = getPlayerData(normalizedUsername);
        return data != null && data.isOperator();
    }

    public static boolean updateOperatorStatus(String username, boolean isOperator) {

        String normalizedUsername = AuthEventHandler.normalizeUsername(username);
        PlayerData oldData = getPlayerData(normalizedUsername);

        if (oldData != null) {

            PlayerData newData = new PlayerData(
                    oldData.getUsername(),
                    oldData.getHashedPassword(),
                    oldData.getRegistrationDate(),
                    oldData.getLastLoginDate(),
                    oldData.getRegistrationIP(),
                    oldData.getLastLoginIP(),
                    oldData.isBanned(),
                    isOperator
            );

            PLAYER_DATA_MAP.put(normalizedUsername, newData);

            saveData();
            AuthMod.logger.info("Updated operator status for player '{}' to {}.", normalizedUsername, isOperator);
            return true;
        }
        AuthMod.logger.warn("Tried to update operator status for unknown player: {}", normalizedUsername);
        return false;
    }

    public static Set<String> getOperatorUsernames() {
        Set<String> operators = new HashSet<>();
        for (Map.Entry<String, PlayerData> entry : PLAYER_DATA_MAP.entrySet()) {
            if (entry.getValue().isOperator()) {
                operators.add(entry.getKey());
            }
        }
        return operators;
    }

    public static void reloadData() throws RuntimeException {
        try {
            AuthMod.logger.info("Reloading player data from {}...", DATA_FILE.getAbsolutePath());

            PLAYER_DATA_MAP.clear();

            loadData();
            AuthMod.logger.info("Player data successfully reloaded. Loaded {} player records.", PLAYER_DATA_MAP.size());
        } catch (Exception e) {
            AuthMod.logger.error("Failed to reload player data", e);

            throw new RuntimeException("Failed to reload player data: " + e.getMessage(), e);
        }
    }
}