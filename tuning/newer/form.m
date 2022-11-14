(* quarter-comma meantone *)

canonicalFormMapping = {{1,0,-4},{0,1,4}}; (* 𝑀ᶜ *)
correspondingCanonicalFormGeneratorMatrix = Transpose[{{1,0,0},{1,0,1/4}}]; (* 𝐺ᶜ *)
projectionDerivedFromCanonicalForm = correspondingCanonicalFormGeneratorMatrix.canonicalFormMapping; (* 𝑃ᶜ *)

canonicalFormMapping //MatrixForm
correspondingCanonicalFormGeneratorMatrix // MatrixForm
projectionDerivedFromCanonicalForm // MatrixForm

transform = IdentityMatrix[2]; (* 𝐹 (octave and twelfth is what canonical form happens to be) *)
transform = {{1,1},{0,1}}; (* octave and fifth *)
transform = {{1,2},{0,-1}}; (* octave and fourth *)
Abs[Det[transform]] == 1 (* assert 𝐹 is unimodular *)

transformedFormMapping = transform.canonicalFormMapping; (* 𝑀 *)
correspondingTransformedFormGeneratorMatrix = correspondingCanonicalFormGeneratorMatrix.Inverse[transform]; (* 𝐺 *)
projectionDerivedFromTransformedForm = correspondingTransformedFormGeneratorMatrix.transformedFormMapping; (* 𝑃 *)

transformedFormMapping //MatrixForm
correspondingTransformedFormGeneratorBasis // MatrixForm
projectionDerivedFromTransformedForm // MatrixForm

projectionDerivedFromCanonicalForm == projectionDerivedFromTransformedForm (* assert 𝑃ᶜ = 𝑃 *)
