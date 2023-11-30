"use strict";

export const _swap = n => m => arr => {
  let res = arr.slice();
  res[m] = arr[n];
  res[n] = arr[m];
  return res;
}

export const _get = n => arr => arr[n]