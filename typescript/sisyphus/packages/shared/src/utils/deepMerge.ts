// Deep merges two objects (simple utility)
export function deepMerge<T>(base: T, upd: Partial<T>): T {
  return { ...base, ...upd };
}
