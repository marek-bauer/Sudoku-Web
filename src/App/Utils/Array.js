"use strict";

export const _swap = n => m => arr => {
  let res = arr.slice();
  res[m] = arr[n];
  res[n] = arr[m];
  return res;
}

export const _get = n => arr => arr[n]

export const _chunks = chunkSize => arr => {
  if (chunkSize <= 0) return [];
  let res = [];
  for (let i=0, len=arr.length; i<len; i+=chunkSize)
    res.push(arr.slice(i,i+chunkSize));
  return res;
}