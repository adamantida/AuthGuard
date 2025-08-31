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

    // ========================================================================
    // Конструктор и геттеры
    // ========================================================================

    public PlayerData(String username, String hashedPassword, long registrationDate) {
        this.username = username;
        this.hashedPassword = hashedPassword;
        this.registrationDate = registrationDate;
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
}