const fs = require('node:fs/promises');

async function readInput() {
  return await fs.readFile('./input.txt', { encoding: 'utf8' });
}

function formatInput(data) {
	const [P, V] = String(data).split('\n')[0].split(' ').map((n) => parseInt(n));
	const [Q, M] = String(data).split('\n')[1].split(' ').map((n) => parseInt(n));
	return [P, V, Q, M];
}

function computeReachableTrees(P, V, Q, M) 
{
	const treesBetweenBuckets = Math.abs(P - Q) - 1;
	const treesRangeDelta = Math.abs(V - M) - 1;
	const maxReachableTreesBetweenBuckets = V + M;
	const maxReachableTrees = 2 * (V + M + 1);

	return maxReachableTrees 
		- ( maxReachableTreesBetweenBuckets 
			- Math.min(
					maxReachableTreesBetweenBuckets, 
					Math.max(
						treesBetweenBuckets, 
						treesRangeDelta
					)
				)
			);
}

readInput().then((data => {
	const [P, V, Q, M] = formatInput(data);
	const reachableTrees = computeReachableTrees(P, V, Q, M);
	fs.writeFile('output.txt', String(reachableTrees));
}));
