/**
 * The following lines intialize dotenv,
 * so that env vars from the .env file are present in process.env
 */
import * as dotenv from 'dotenv';
dotenv.config();

export const sum = (a: number, b: number): number => {
  console.log(a);
  return a + b;
};
 
sum(1, 2);
