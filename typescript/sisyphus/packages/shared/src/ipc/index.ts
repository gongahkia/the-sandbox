import { AppConfig } from '../types/AppConfig';
import { PluginMeta } from '../types/Plugin';

/** Requests config */
export interface IPCSettingsGet {
  key?: keyof AppConfig;
}

/** Sets config */
export interface IPCSettingsSet {
  key: keyof AppConfig;
  value: any;
}

/** Loads a plugin */
export interface IPCPluginLoad {
  meta: PluginMeta;
}

/** IPC channel map */
export type IPCRequestMap = {
  'settings:get': IPCSettingsGet;
  'settings:set': IPCSettingsSet;
  'plugin:load': IPCPluginLoad;
  // Extend as needed
};
