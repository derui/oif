
const MAX_COUNT = 30;

let count = 0;

async function main() {
  const promise = new Promise((resolve) => {
    const id = setInterval(() => {
      console.log(`count ${count}`);

      count++;
      if (count >= MAX_COUNT) {
        clearInterval(id);
        resolve();
      }
    }, 200);;
  })
}

await main()
