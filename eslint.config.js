import js from '@eslint/js';
import { defineConfig } from 'eslint/config';
import globals from 'globals';

export default defineConfig([
  {
    languageOptions: { globals: globals.node },
    plugins: { js },
    extends: ['js/recommended'],
    files: ['**/*.{js,mjs,cjs}'],
  },
]);
