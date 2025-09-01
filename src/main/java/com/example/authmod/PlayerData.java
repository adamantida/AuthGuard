package com.example.authmod;

/**
 * Класс данных игрока для хранения информации об авторизации.
 */
public class PlayerData {

    /** Имя пользователя */
    private final String username;

    /** Хеш пароля */
    private final String hashedPassword;

    /** Дата регистрации */
    private final long registrationDate;

    /** IP-адрес регистрации (новое поле) */
    private final String registrationIP;

    /** Последний IP-адрес входа (новое поле) */
    private final String lastLoginIP;

    /** Статус бана (новое поле) */
    private final boolean isBanned;

    // ========================================================================
    // Конструкторы и геттеры
    // ========================================================================

    /**
     * Конструктор для новых записей
     */
    public PlayerData(String username, String hashedPassword, long registrationDate,
                      String registrationIP, String lastLoginIP, boolean isBanned) {
        this.username = username;
        this.hashedPassword = hashedPassword;
        this.registrationDate = registrationDate;
        this.registrationIP = registrationIP;
        this.lastLoginIP = lastLoginIP;
        this.isBanned = isBanned;
    }

    /**
     * Конструктор для совместимости со старыми данными
     */
    public PlayerData(String username, String hashedPassword, long registrationDate) {
        this(username, hashedPassword, registrationDate, "unknown", "unknown", false);
    }

    public String getUsername() {
        return username;
    }

    public String getHashedPassword() {
        return hashedPassword;
    }

    public long getRegistrationDate() {
        return registrationDate;
    }

    public String getRegistrationIP() {
        return registrationIP;
    }

    public String getLastLoginIP() {
        return lastLoginIP;
    }

    public boolean isBanned() {
        return isBanned;
    }
}