from qiskit.primitives import BaseSampler #This is Alias for BaseSamplerV1
from qiskit.primitives.sampler import SamplerResult
from qiskit.result import QuasiDistribution
from qiskit.quantum_info import Statevector
from qiskit.circuit import QuantumCircuit
from typing import Optional, Sequence, Union
from qiskit.primitives.primitive_job import PrimitiveJob  # add this at the top

class IdealSampler(BaseSampler):
    def __init__(self):
        super().__init__()
        self._circuits = []

    def _run(
        self,
        circuits: Sequence[Union[QuantumCircuit, int]],
        parameter_values: Optional[Sequence[Sequence[float]]] = None,
        **run_options
    ):
        if parameter_values is None:
            parameter_values = [()] * len(circuits)

        resolved_circuits = []
        for circ in circuits:
            if isinstance(circ, int):
                resolved_circuits.append(self._circuits[circ])
            else:
                self._circuits.append(circ)
                resolved_circuits.append(circ)

        shots = run_options.get("shots", 1024)
        results = []

        for circuit, params in zip(resolved_circuits, parameter_values):
            if circuit.num_parameters > 0:
                bound = circuit.assign_parameters(dict(zip(circuit.parameters, params)))
            else:
                bound = circuit

            bound = bound.remove_final_measurements(inplace=False)
            sv = Statevector(bound)
            probs = sv.probabilities_dict()

            n = bound.num_qubits
            full_probs = {
                f"{int(k, 2):0{n}b}" if isinstance(k, str) else f"{k:0{n}b}": v
                for k, v in probs.items()
            }

            counts = {k: int(round(v * shots)) for k, v in full_probs.items()}
            total = sum(counts.values())
            quasi = QuasiDistribution({k: v / total for k, v in counts.items()})
            results.append(quasi)

        metadata = [{} for _ in results]
        job = PrimitiveJob(lambda: SamplerResult(quasi_dists=results, metadata=metadata))
        job._submit()
        return job

        #return SamplerResult(quasi_dists=results, metadata=metadata)



from qiskit_algorithms import QAOA

from qiskit_optimization.applications import Maxcut
from qiskit_optimization.converters import QuadraticProgramToQubo
from qiskit_optimization.algorithms import MinimumEigenOptimizer
from qiskit_algorithms.utils import algorithm_globals  # ✅
from qiskit.circuit.library import TwoLocal
from qiskit_optimization import QuadraticProgram
from qiskit_algorithms.optimizers import COBYLA
# Set random seed
algorithm_globals.random_seed = 42

# Define MaxCut problem (triangle)
edges = [(0, 1), (1, 2), (0, 2)]
maxcut = Maxcut(graph=edges)
problem: QuadraticProgram = maxcut.to_quadratic_program()

# QAOA with your DeterministicSampler and COBYLA optimizer
qaoa = QAOA(
    sampler=IdealSampler(),
    optimizer=COBYLA(maxiter=100),
    reps=1
)

# Solve using MinimumEigenOptimizer
optimizer = MinimumEigenOptimizer(qaoa)
result = optimizer.solve(problem)

print("Optimal solution:", result.x)
print("Objective value:", result.fval)


from qiskit import QuantumCircuit

# Create a Bell state circuit
qc = QuantumCircuit(2)
qc.h(0)
qc.cx(0, 1)
qc.measure_all()

# Instantiate your DeterministicSampler
sampler = IdealSampler()

# Run the circuit (returns a PrimitiveJob)
job = sampler.run([qc], shots=1000)

# Get the SamplerResult
result = job.result()

# Access quasi-distribution
print("Probabilities (normalized):")
print(result.quasi_dists[0])  # Example: {'00': 0.5, '11': 0.5}

# Convert to deterministic counts
counts = {k: int(v * 1000) for k, v in result.quasi_dists[0].items()}
print("\nDeterministic counts (probs × shots):")
print(counts)  # Example: {'00': 500, '11': 500}

