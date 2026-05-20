## Minimal PCG-XSH-RR implementation for stable, cross-backend PRNG.
## 64-bit state, 32-bit output.

type
  Pcg* = object
    state: uint64
    inc: uint64

proc next*(rng: var Pcg): uint32 =
  ## Generates the next 32-bit random unsigned integer.
  let oldState = rng.state
  # Advance internal state
  # Multiplier from PCG reference implementation
  rng.state = oldState * 6364136223846793005'u64 + rng.inc
  
  # Calculate output function (XSH RR)
  let xorshifted = uint32(((oldState shr 18) xor oldState) shr 27)
  let rot = uint32(oldState shr 59)
  return (xorshifted shr rot) or (xorshifted shl (cast[uint32](-cast[int32](rot)) and 31'u32))

proc initPcg*(seed: uint64, seq: uint64 = 0): Pcg =
  ## Initializes the PCG RNG with a seed and a sequence (stream) ID.
  result.inc = (seq shl 1) or 1'u64
  
  # Use splitmix64-style mixer for the seed to ensure avalanche effect
  var x = seed + 0x9E3779B97F4A7C15'u64
  x = (x xor (x shr 30)) * 0xBF58476D1CE4E5B9'u64
  x = (x xor (x shr 27)) * 0x94D049BB133111EB'u64
  x = x xor (x shr 31)
  
  result.state = x + result.inc
  discard result.next()
