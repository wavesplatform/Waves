// Picks the seed for test/helpers/randomData.ts once per run and prints it; rerun with RIDE_TEST_SEED=<n> to reproduce.
export default function setup(): void {
  process.env.RIDE_TEST_SEED ??= String(Math.floor(Math.random() * 2 ** 32));
  console.log(`RIDE_TEST_SEED=${process.env.RIDE_TEST_SEED}`);
}
