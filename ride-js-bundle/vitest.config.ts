import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    environment: "node",
    globals: false,
    include: ["test/**/*.test.ts"],
    globalSetup: ["test/globalSetup.ts"],
    // The first test in each file loads the 6.7 MB ScalaJS module; with one worker per core that alone can pass 5 s.
    testTimeout: 60_000
  }
});
