/* tslint:disable */
/* eslint-disable */
export const memory: WebAssembly.Memory;
export const check_program: (a: number, b: number) => [number, number, number, number];
export const enable_effect_log: (a: number) => void;
export const eval_program: (a: number, b: number) => [number, number, number, number];
export const eval_ui: (a: number, b: number) => [number, number];
export const eval_ui_checked: (a: number, b: number) => [number, number];
export const eval_with_output: (a: number, b: number) => [number, number, number, number];
export const get_effect_log: () => [number, number];
export const infer_type: (a: number, b: number) => [number, number, number, number];
export const init_dom_bridge: (a: any) => void;
export const invoke_callback: (a: number) => void;
export const clear_effect_log: () => void;
export const reset_runtime: () => void;
export const __wbindgen_malloc: (a: number, b: number) => number;
export const __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
export const __wbindgen_exn_store: (a: number) => void;
export const __externref_table_alloc: () => number;
export const __wbindgen_externrefs: WebAssembly.Table;
export const __externref_table_dealloc: (a: number) => void;
export const __wbindgen_free: (a: number, b: number, c: number) => void;
export const __wbindgen_start: () => void;
