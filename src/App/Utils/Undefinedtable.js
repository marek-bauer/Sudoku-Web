export const convert_undefined = wrap => nothing => val => {
  if (val === undefined) {
    return nothing;
  } else {
    return wrap(val);
  }
}