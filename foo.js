let b = 0;
for (let i = 0; i < 1000000; i++) {
  if (b % 10000 === 0) {
    console.log(b);
  }
  b++;
}
console.log(b);
