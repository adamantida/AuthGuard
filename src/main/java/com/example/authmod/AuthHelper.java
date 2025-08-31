package com.example.authmod;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.nio.charset.StandardCharsets;

/**
 * Вспомогательный класс для хеширования паролей.
 * Использует SHA-256 с солью для безопасного хранения паролей.
 */
public class AuthHelper {

    /** Генератор случайных чисел для создания соли */
    private static final SecureRandom RANDOM = new SecureRandom();

    /** Массив символов для hex-кодирования */
    private static final char[] HEX_ARRAY = "0123456789ABCDEF".toCharArray();

    /** Количество итераций для хеширования */
    private static final int HASH_ITERATIONS = 10000;

    // ========================================================================
    // Основные методы
    // ========================================================================

    /**
     * Хеширует пароль с использованием соли и SHA-256
     */
    public static String hashPassword(String password) {
        try {
            // Генерируем соль
            byte[] salt = new byte[16];
            RANDOM.nextBytes(salt);

            // Создаем хэш с солью и множественными итерациями
            byte[] hashedPassword = hashWithSalt(password, salt);

            // Объединяем соль и хэш для хранения
            byte[] combined = new byte[salt.length + hashedPassword.length];
            System.arraycopy(salt, 0, combined, 0, salt.length);
            System.arraycopy(hashedPassword, 0, combined, salt.length, hashedPassword.length);

            // Кодируем в hex-строку
            return bytesToHex(combined);
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException("Ошибка хэширования пароля", e);
        }
    }

    /**
     * Проверяет пароль на соответствие сохраненному хешу
     */
    public static boolean verifyPassword(String password, String storedHash) {
        try {
            if (storedHash == null || storedHash.length() % 2 != 0) {
                return false;
            }

            // Декодируем из hex
            byte[] combined = hexToBytes(storedHash);

            // Извлекаем соль и хэш
            byte[] salt = new byte[16];
            byte[] originalHash = new byte[combined.length - 16];
            System.arraycopy(combined, 0, salt, 0, 16);
            System.arraycopy(combined, 16, originalHash, 0, originalHash.length);

            // Вычисляем хэш введенного пароля с той же солью
            byte[] testHash = hashWithSalt(password, salt);

            // Сравниваем хэши
            return MessageDigest.isEqual(originalHash, testHash);
        } catch (NoSuchAlgorithmException e) {
            AuthMod.logger.error("Password verification failed", e);
            return false;
        }
    }

    // ========================================================================
    // Вспомогательные методы
    // ========================================================================

    /**
     * Вычисляет хэш пароля с использованием соли и множественных итераций
     */
    private static byte[] hashWithSalt(String password, byte[] salt) throws NoSuchAlgorithmException {
        MessageDigest md = MessageDigest.getInstance("SHA-256");

        // Используем UTF-8 для корректной обработки кириллицы
        byte[] passwordBytes = password.getBytes(StandardCharsets.UTF_8);

        // Добавляем соль
        md.update(salt);

        // Выполняем несколько итераций для увеличения безопасности
        byte[] hashed = md.digest(passwordBytes);
        for (int i = 0; i < HASH_ITERATIONS - 1; i++) {
            md.reset();
            hashed = md.digest(hashed);
        }

        return hashed;
    }

    /**
     * Преобразует массив байтов в hex-строку
     */
    private static String bytesToHex(byte[] bytes) {
        char[] hexChars = new char[bytes.length * 2];
        for (int i = 0; i < bytes.length; i++) {
            int v = bytes[i] & 0xFF;
            hexChars[i * 2] = HEX_ARRAY[v >>> 4];
            hexChars[i * 2 + 1] = HEX_ARRAY[v & 0x0F];
        }
        return new String(hexChars);
    }

    /**
     * Преобразует hex-строку в массив байтов
     */
    private static byte[] hexToBytes(String hex) {
        int len = hex.length();
        byte[] data = new byte[len / 2];
        for (int i = 0; i < len; i += 2) {
            data[i / 2] = (byte) ((Character.digit(hex.charAt(i), 16) << 4)
                    + Character.digit(hex.charAt(i + 1), 16));
        }
        return data;
    }
}