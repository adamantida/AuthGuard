package com.example.authmod;

import cpw.mods.fml.common.FMLCommonHandler;
import cpw.mods.fml.common.Mod;
import cpw.mods.fml.common.Mod.EventHandler;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;
import cpw.mods.fml.common.event.FMLServerStoppingEvent;
import cpw.mods.fml.common.event.FMLServerStartingEvent;
import net.minecraftforge.common.MinecraftForge;
import org.apache.logging.log4j.Logger;

/**
 * Основной класс мода аутентификации.
 * Регистрирует обработчики событий и команды.
 */
@Mod(modid = "authmod", name = "Auth Mod", version = "1.0", acceptableRemoteVersions = "*")
public class AuthMod {

    /** Логгер для отладки и информации */
    public static Logger logger;

    // ========================================================================
    // Обработка событий мода
    // ========================================================================

    @EventHandler
    public void preInit(FMLPreInitializationEvent event) {
        logger = event.getModLog();
        PlayerDataManager.init(event.getModConfigurationDirectory());

        // Регистрируем на ОБОИХ EventBus (ключевое для 1.7.10)
        FMLCommonHandler.instance().bus().register(new AuthEventHandler());
        FMLCommonHandler.instance().bus().register(new ChatEventHandler());
        MinecraftForge.EVENT_BUS.register(new AuthEventHandler());
        MinecraftForge.EVENT_BUS.register(new ChatEventHandler());

        logger.info("AuthMod initialized - Security Level: HIGH");
        logger.info("Supported commands: /auth register, /auth login");
        logger.info("Auto-kick timeout: {} seconds", AuthEventHandler.MAX_LOGIN_TIME / 1000);
    }

    @EventHandler
    public void serverLoad(FMLServerStartingEvent event) {
        event.registerServerCommand(new AuthCommand());
        MinecraftForge.EVENT_BUS.register(new ChatEventHandler());

        logger.info("AuthMod loaded successfully!");
    }

    @EventHandler
    public void serverStopping(FMLServerStoppingEvent event) {
        AuthEventHandler.shutdown();
        logger.info("AuthMod shut down successfully");
    }
}