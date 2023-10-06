import { parsers as baseParsers } from "./base.mjs";
import { createRequire } from "module";

export { languages, printers } from "./base.mjs";

const require = createRequire(import.meta.url);

const liquidsoap = require("../dist/liquidsoap.cjs");

export const parsers = {
  liquidsoap: {
    ...baseParsers.liquidsoap,
    parse: liquidsoap.lang.parse,
  },
};
