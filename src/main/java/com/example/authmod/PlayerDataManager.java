package com.example.authmod;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.reflect.TypeToken;

import java.io.*;
import java.lang.reflect.Type;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Менеджер данных игроков.
 * Отвечает за сохранение и загрузку данных об авторизации.
 */
public class PlayerDataManager {

    /** Gson для сериализации/десериализации */
    private static final Gson GSON = new GsonBuilder().setPrettyPrinting().create();

    /** Файл для хранения данных */
    private static File DATA_FILE;

    /** Карта данных игроков */
    private static final Map<String, PlayerData> PLAYER_DATA_MAP = new ConcurrentHashMap<>();

    /** Задержка перед сохранением (10 секунд) */
    private static final int SAVE_DELAY = 20 * 10;

    /** Флаг необходимости сохранения */
    private static final AtomicBoolean savePending = new AtomicBoolean(false);

    /** Пул потоков для отложенного сохранения */
    private static final ScheduledExecutorService SAVE_EXECUTOR =
            Executors.newSingleThreadScheduledExecutor(r -> {
                Thread t = new Thread(r);
                t.setName("Auth-Save-Worker");
                t.setDaemon(true);
                return t;
            });

    // ========================================================================
    // Основные методы
    // ========================================================================

    /**
     * Инициализирует менеджер данных
     */
    public static void init(File configDir) {
        DATA_FILE = new File(configDir, "authdata.json");
        loadData();
    }

    /**
     * Проверяет, зарегистрирован ли игрок
     */
    public static boolean isPlayerRegistered(String username) {
        return PLAYER_DATA_MAP.containsKey(username.toLowerCase());
    }

    /**
     * Возвращает хеш пароля игрока
     */
    public static String getPlayerHash(String username) {
        PlayerData data = PLAYER_DATA_MAP.get(username.toLowerCase());
        return (data != null) ? data.getHashedPassword() : null;
    }

    /**
     * Регистрирует нового игрока
     */
    public static void registerPlayer(String username, String password, String ip) {
        String hashedPassword = AuthHelper.hashPassword(password);
        PlayerData data = new PlayerData(username, hashedPassword, System.currentTimeMillis(),
                ip, ip, false);
        PLAYER_DATA_MAP.put(username.toLowerCase(), data);
        scheduleSave();
    }

    /**
     * Обновляет данные о последнем входе
     */
    public static void updateLoginData(String username, String ip) {
        PlayerData oldData = PLAYER_DATA_MAP.get(username.toLowerCase());
        if (oldData != null) {
            PlayerData newData = new PlayerData(
                    oldData.getUsername(),
                    oldData.getHashedPassword(),
                    oldData.getRegistrationDate(),
                    oldData.getRegistrationIP(),
                    ip,
                    oldData.isBanned()
            );
            PLAYER_DATA_MAP.put(username.toLowerCase(), newData);
            scheduleSave();
        }
    }

    /**
     * Возвращает данные игрока
     */
    public static PlayerData getPlayerData(String username) {
        return PLAYER_DATA_MAP.get(username.toLowerCase());
    }

    /**
     * Сбрасывает пароль игрока
     */
    public static boolean resetPlayerPassword(String username) {
        PlayerData oldData = PLAYER_DATA_MAP.get(username.toLowerCase());
        if (oldData != null) {
            // Генерируем временный пароль или оставляем без изменений
            String tempPassword = "temp1234"; // Можно сделать более безопасным
            String hashedPassword = AuthHelper.hashPassword(tempPassword);

            PlayerData newData = new PlayerData(
                    oldData.getUsername(),
                    hashedPassword,
                    oldData.getRegistrationDate(),
                    oldData.getRegistrationIP(),
                    oldData.getLastLoginIP(),
                    oldData.isBanned()
            );

            PLAYER_DATA_MAP.put(username.toLowerCase(), newData);
            scheduleSave();
            return true;
        }
        return false;
    }

    public static boolean updatePassword(String username, String newPassword) {
        PlayerData oldData = PLAYER_DATA_MAP.get(username.toLowerCase());
        if (oldData != null) {
            String hashedPassword = AuthHelper.hashPassword(newPassword);
            PlayerData newData = new PlayerData(
                    oldData.getUsername(),
                    hashedPassword,
                    oldData.getRegistrationDate(),
                    oldData.getRegistrationIP(),
                    oldData.getLastLoginIP(),
                    oldData.isBanned()
            );
            PLAYER_DATA_MAP.put(username.toLowerCase(), newData);
            scheduleSave();
            return true;
        }
        return false;
    }
    public static void setPlayerBanned(String username, boolean banned) {
        PlayerData oldData = PLAYER_DATA_MAP.get(username.toLowerCase());
        if (oldData != null) {
            PlayerData newData = new PlayerData(
                    oldData.getUsername(),
                    oldData.getHashedPassword(),
                    oldData.getRegistrationDate(),
                    oldData.getRegistrationIP(),
                    oldData.getLastLoginIP(),
                    banned
            );
            PLAYER_DATA_MAP.put(username.toLowerCase(), newData);
            scheduleSave();
        }
    }
    /**
     * Получает список всех игроков
     */
    public static List<PlayerData> getAllPlayers() {
        return new ArrayList<>(PLAYER_DATA_MAP.values());
    }

    // ========================================================================
    // Вспомогательные методы
    // ========================================================================

    /**
     * Загружает данные из файла
     */
    private static void loadData() {
        if (DATA_FILE.exists()) {
            try (Reader reader = new FileReader(DATA_FILE)) {
                // Используем JsonParser для обработки старых данных
                JsonObject json = new JsonParser().parse(reader).getAsJsonObject();
                Map<String, PlayerData> loadedData = new HashMap<>();

                for (Map.Entry<String, JsonElement> entry : json.entrySet()) {
                    String username = entry.getKey();
                    JsonObject playerJson = entry.getValue().getAsJsonObject();

                    // Проверяем наличие новых полей
                    String registrationIP = "unknown";
                    String lastLoginIP = "unknown";
                    boolean isBanned = false;

                    if (playerJson.has("registrationIP")) {
                        registrationIP = playerJson.get("registrationIP").getAsString();
                    }

                    if (playerJson.has("lastLoginIP")) {
                        lastLoginIP = playerJson.get("lastLoginIP").getAsString();
                    }

                    if (playerJson.has("isBanned")) {
                        isBanned = playerJson.get("isBanned").getAsBoolean();
                    }

                    // Создаем данные с учетом обратной совместимости
                    PlayerData data = new PlayerData(
                            username,
                            playerJson.get("hashedPassword").getAsString(),
                            playerJson.get("registrationDate").getAsLong(),
                            registrationIP,
                            lastLoginIP,
                            isBanned
                    );

                    loadedData.put(username.toLowerCase(), data);
                }

                if (!loadedData.isEmpty()) {
                    PLAYER_DATA_MAP.clear();
                    PLAYER_DATA_MAP.putAll(loadedData);
                }
            } catch (Exception e) {
                AuthMod.logger.error("Failed to load player data", e);
                // Инициализируем пустую карту при ошибке
                PLAYER_DATA_MAP.clear();
            }
        }
    }

    /**
     * Планирует сохранение данных
     */
    private static void scheduleSave() {
        if (savePending.getAndSet(true)) {
            return; // Сохранение уже запланировано
        }

        SAVE_EXECUTOR.schedule(() -> {
            savePending.set(false);
            saveData();
        }, SAVE_DELAY, TimeUnit.MILLISECONDS);
    }

    /**
     * Сохраняет данные в файл
     */
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
}