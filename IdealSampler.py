from qiskit.primitives import BaseSamplerV2
from qiskit.quantum_info import Statevector
from qiskit.circuit import QuantumCircuit
from qiskit.primitives.utils import _circuit_key
from types import SimpleNamespace
from typing import List, Optional, Sequence, Union
import copy


class DeterministicCounts:
    """Returns exact counts from probabilities × shots (no randomness)."""
    def __init__(self, quasi_dist, shots):
        self._counts = {
            k: int(round(v * shots)) for k, v in quasi_dist.items()
        }

    def get_counts(self):
        return self._counts


class IdealSampler(BaseSamplerV2):
    def __init__(self, circuits: Optional[Sequence[QuantumCircuit]] = None):
        self._circuits = circuits or []
        self._circuit_ids = {_circuit_key(circ): i for i, circ in enumerate(self._circuits)}

    def _get_or_add_circuit(self, circuit: QuantumCircuit) -> int:
        key = _circuit_key(circuit)
        if key in self._circuit_ids:
            return self._circuit_ids[key]
        else:
            idx = len(self._circuits)
            self._circuits.append(circuit)
            self._circuit_ids[key] = idx
            return idx

    def run(
        self,
        circuits: Sequence[Union[int, QuantumCircuit]],
        parameter_values: Optional[Sequence[Sequence[float]]] = None,
        **run_options,
    ):
        resolved_circuits = []

        for circ in circuits:
            if isinstance(circ, int):
                resolved_circuits.append(copy.deepcopy(self._circuits[circ]))
            else:
                resolved_circuits.append(circ)

        if parameter_values is None:
            parameter_values = [()] * len(resolved_circuits)

        shots = run_options.get("shots", 1024)

        results = []
        for circuit, params in zip(resolved_circuits, parameter_values):
            if circuit.num_parameters > 0:
                param_dict = dict(zip(circuit.parameters, params))
                circuit = circuit.assign_parameters(param_dict, inplace=False)

            # Remove measurements
            circuit = circuit.remove_final_measurements(inplace=False)

            # Get statevector and probabilities
            sv = Statevector(circuit)
            probs = sv.probabilities_dict()

            # Convert keys to full bitstrings
            n = circuit.num_qubits
            full_probs = {
                f"{int(k, 2):0{n}b}" if isinstance(k, str) else f"{k:0{n}b}": v
                for k, v in probs.items()
            }

            counts = DeterministicCounts(full_probs, shots)
            result = SimpleNamespace(data=SimpleNamespace(meas=counts))
            results.append(result)

        return results

    def circuits(self) -> List[QuantumCircuit]:
        return self._circuits


if __name__ == "__main__":
    qc = QuantumCircuit(2)
    qc.h(0)
    qc.cx(0, 1)
    qc.measure_all()

    sampler = NufSampler()
    result = sampler.run([qc], shots=1024)
    print(result[0].data.meas.get_counts())  # {'00': 500, '11': 500}

