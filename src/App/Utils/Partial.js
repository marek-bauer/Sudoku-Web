export const _partial = f => x =>
{
  try {
    return f(x);
  } catch {
    return undefined;
  }
}