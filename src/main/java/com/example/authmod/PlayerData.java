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

    private final boolean isOperator;




    /**
     * Конструктор для создания нового объекта PlayerData.
     *
     * @param username Имя пользователя
     * @param hashedPassword Хеш пароля
     * @param registrationDate Дата регистрации
     * @param registrationIP IP-адрес регистрации
     * @param lastLoginIP Последний IP-адрес входа
     * @param isBanned Статус бана
     * @param isOperator Статус оператора (новый параметр)
     */
    public PlayerData(String username, String hashedPassword, long registrationDate,
                      String registrationIP, String lastLoginIP, boolean isBanned, boolean isOperator) {
        this.username = username;
        this.hashedPassword = hashedPassword;
        this.registrationDate = registrationDate;
        this.registrationIP = registrationIP;
        this.lastLoginIP = lastLoginIP;
        this.isBanned = isBanned;
        this.isOperator = isOperator;
    }

    /**
     * Конструктор для совместимости со старыми данными
     */
    public PlayerData(String username, String hashedPassword, long registrationDate) {
        this(username, hashedPassword, registrationDate, "unknown", "unknown", false, false);
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
    public boolean isOperator() {
        return isOperator;
    }
}