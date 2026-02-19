/* tslint:disable */
/* eslint-disable */

/**
 * Type-check Loon source and return diagnostics.
 */
export function check_program(source: string): string;

/**
 * Clear the effect log.
 */
export function clear_effect_log(): void;

/**
 * Enable or disable effect logging.
 */
export function enable_effect_log(enabled: boolean): void;

/**
 * Evaluate a Loon program and return the result as a string.
 */
export function eval_program(source: string): string;

/**
 * Evaluate a Loon UI program. The DOM bridge must be initialized first.
 */
export function eval_ui(source: string): void;

/**
 * Evaluate a Loon UI program, returning structured JSON instead of throwing.
 * Success: {"ok":true}
 * Error:   {"ok":false,"error":{"message":"...","span":[start,end],"stack":[{"fn":"name","span":[start,end]},...]}}
 */
export function eval_ui_checked(source: string): string;

/**
 * Evaluate Loon source and capture all println output.
 */
export function eval_with_output(source: string): string;

/**
 * Get the effect log as a JSON array.
 */
export function get_effect_log(): string;

/**
 * Infer the type of the last named binding (fn/let) in the source.
 */
export function infer_type(source: string): string;

/**
 * Initialize the DOM bridge from JS. The bridge is a JS function that receives
 * (operation: string, args: any[]) and returns a result.
 * Must be called before eval_ui.
 */
export function init_dom_bridge(bridge: Function): void;

/**
 * Invoke a stored Loon callback by ID (called from JS event handlers).
 */
export function invoke_callback(id: number): void;

/**
 * Reset the Loon runtime state (callbacks, etc.) for hot reload.
 */
export function reset_runtime(): void;

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
    readonly memory: WebAssembly.Memory;
    readonly check_program: (a: number, b: number) => [number, number, number, number];
    readonly enable_effect_log: (a: number) => void;
    readonly eval_program: (a: number, b: number) => [number, number, number, number];
    readonly eval_ui: (a: number, b: number) => [number, number];
    readonly eval_ui_checked: (a: number, b: number) => [number, number];
    readonly eval_with_output: (a: number, b: number) => [number, number, number, number];
    readonly get_effect_log: () => [number, number];
    readonly infer_type: (a: number, b: number) => [number, number, number, number];
    readonly init_dom_bridge: (a: any) => void;
    readonly invoke_callback: (a: number) => void;
    readonly clear_effect_log: () => void;
    readonly reset_runtime: () => void;
    readonly __wbindgen_malloc: (a: number, b: number) => number;
    readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
    readonly __wbindgen_exn_store: (a: number) => void;
    readonly __externref_table_alloc: () => number;
    readonly __wbindgen_externrefs: WebAssembly.Table;
    readonly __externref_table_dealloc: (a: number) => void;
    readonly __wbindgen_free: (a: number, b: number, c: number) => void;
    readonly __wbindgen_start: () => void;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;

/**
 * Instantiates the given `module`, which can either be bytes or
 * a precompiled `WebAssembly.Module`.
 *
 * @param {{ module: SyncInitInput }} module - Passing `SyncInitInput` directly is deprecated.
 *
 * @returns {InitOutput}
 */
export function initSync(module: { module: SyncInitInput } | SyncInitInput): InitOutput;

/**
 * If `module_or_path` is {RequestInfo} or {URL}, makes a request and
 * for everything else, calls `WebAssembly.instantiate` directly.
 *
 * @param {{ module_or_path: InitInput | Promise<InitInput> }} module_or_path - Passing `InitInput` directly is deprecated.
 *
 * @returns {Promise<InitOutput>}
 */
export default function __wbg_init (module_or_path?: { module_or_path: InitInput | Promise<InitInput> } | InitInput | Promise<InitInput>): Promise<InitOutput>;
