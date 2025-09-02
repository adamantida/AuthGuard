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

    public static Logger logger;

    @EventHandler
    public void preInit(FMLPreInitializationEvent event) {
        logger = event.getModLog();
        PlayerDataManager.init(event.getModConfigurationDirectory());

        FMLCommonHandler.instance().bus().register(new AuthEventHandler());
        FMLCommonHandler.instance().bus().register(new ChatEventHandler());
        FMLCommonHandler.instance().bus().register(new BanStatusSyncer());
        MinecraftForge.EVENT_BUS.register(new AuthEventHandler());
        MinecraftForge.EVENT_BUS.register(new ChatEventHandler());
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