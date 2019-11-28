pub fn discriminants_equal<T>(expected: &T, actual: &T) -> bool {
    let expected_discriminant = std::mem::discriminant(expected);
    let actual_discriminant = std::mem::discriminant(actual);
    expected_discriminant == actual_discriminant
}
