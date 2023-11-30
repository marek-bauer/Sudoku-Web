import __wbg_init, {Marek} from "wasm";


export const _new = function () {
  return function (onError,onSuccess) {
    __wbg_init().then(w => onSuccess(Marek.new()))
    return function (cancelError, onCancelerError, onCancelerSuccess) {
      onCancelerSuccess();
    } 
  } 
}

export const _next = test => {
  return test.next();
}