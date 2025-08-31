package com.example.authmod;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;

import java.io.*;
import java.lang.reflect.Type;
import java.util.Map;
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
    public static void registerPlayer(String username, String password) {
        String hashedPassword = AuthHelper.hashPassword(password);
        PlayerData data = new PlayerData(username, hashedPassword, System.currentTimeMillis());
        PLAYER_DATA_MAP.put(username.toLowerCase(), data);
        scheduleSave();
    }

    /**
     * Проверяет пароль игрока
     */
    public static boolean checkPassword(String username, String password) {
        PlayerData data = PLAYER_DATA_MAP.get(username.toLowerCase());
        return data != null && AuthHelper.verifyPassword(password, data.getHashedPassword());
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
                Type type = new TypeToken<Map<String, PlayerData>>() {}.getType();
                Map<String, PlayerData> loadedData = GSON.fromJson(reader, type);

                if (loadedData != null) {
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
}