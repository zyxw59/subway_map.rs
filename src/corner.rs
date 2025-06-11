use crate::values::UnitVector;

/// Calculates the offsets from the corner along each ray of a corner, with the given transverse
/// offsets on each ray.
///
/// Positive transverse offsets correspond to shifting the ray to the right relative to their
/// respective rays.
///
/// Positive longitudinal offsets correspond to shifting the corner along the respective rays.
///
#[doc = include_str!("doc-calculate-longitudinal-offsets.svg")]
#[inline]
pub fn calculate_longitudinal_offsets(
    in_dir: UnitVector,
    out_dir: UnitVector,
    transverse_in: f64,
    transverse_out: f64,
) -> (f64, f64) {
    // positive if in_dir is clockwise of out_dir
    let sin = out_dir.cross(*in_dir);
    let cos = *in_dir * *out_dir;
    let transverse_in = -transverse_in;
    (
        transverse_in.mul_add(cos, transverse_out) / sin,
        transverse_out.mul_add(cos, transverse_in) / sin,
    )
}

/// Calculate `|tan(θ/2)|`, where `θ` is the angle formed by the two vectors.
#[inline]
pub fn calculate_tan_half_angle(in_dir: UnitVector, out_dir: UnitVector) -> f64 {
    let cos = *in_dir * *out_dir;
    ((1.0 - cos) / (1.0 + cos)).sqrt()
}

#[cfg(test)]
mod tests {
    use float_eq::assert_float_eq;

    use crate::values::UnitVector;

    use super::calculate_longitudinal_offsets;

    #[test]
    fn longitudinal_offsets() {
        #[derive(Debug)]
        struct Case {
            in_dir: UnitVector,
            out_dir: UnitVector,
            trans_in: f64,
            trans_out: f64,
            long_in: f64,
            long_out: f64,
        }

        let test_cases = [
            Case {
                in_dir: UnitVector::EAST,
                out_dir: UnitVector::SOUTH,
                trans_in: 0.0,
                trans_out: 0.0,
                long_in: 0.0,
                long_out: 0.0,
            },
            Case {
                in_dir: UnitVector::EAST,
                out_dir: UnitVector::SOUTH,
                trans_in: 6.0,
                trans_out: 7.0,
                long_in: 7.0,
                long_out: -6.0,
            },
            Case {
                in_dir: UnitVector::EAST,
                out_dir: UnitVector::SOUTH,
                trans_in: -6.0,
                trans_out: 7.0,
                long_in: 7.0,
                long_out: 6.0,
            },
            Case {
                in_dir: UnitVector::EAST,
                out_dir: UnitVector::NORTH,
                trans_in: 6.0,
                trans_out: 7.0,
                long_in: -7.0,
                long_out: 6.0,
            },
            Case {
                in_dir: UnitVector::NORTH,
                out_dir: UnitVector::dir(120.0),
                trans_in: 2.0,
                trans_out: 4.0,
                long_in: -10.0 / 3f64.sqrt(),
                long_out: 8.0 / 3f64.sqrt(),
            },
            Case {
                in_dir: UnitVector::NORTH,
                out_dir: UnitVector::dir(-120.0),
                trans_in: 2.0,
                trans_out: 4.0,
                long_in: 10.0 / 3f64.sqrt(),
                long_out: -8.0 / 3f64.sqrt(),
            },
            Case {
                in_dir: UnitVector::NORTH,
                out_dir: UnitVector::dir(120.0),
                trans_in: -2.0,
                trans_out: 4.0,
                long_in: -2.0 * 3f64.sqrt(),
                long_out: 0.0,
            },
            Case {
                in_dir: UnitVector::NORTH,
                out_dir: UnitVector::dir(120.0),
                trans_in: 2.0,
                trans_out: -4.0,
                long_in: 2.0 * 3f64.sqrt(),
                long_out: 0.0,
            },
            Case {
                in_dir: UnitVector::NORTH,
                out_dir: UnitVector::dir(120.0),
                trans_in: -2.0,
                trans_out: -4.0,
                long_in: 10.0 / 3f64.sqrt(),
                long_out: -8.0 / 3f64.sqrt(),
            },
        ];

        const TOLERANCE: f64 = 1e-10;

        for case in test_cases {
            let (actual_long_in, actual_long_out) = calculate_longitudinal_offsets(
                case.in_dir,
                case.out_dir,
                case.trans_in,
                case.trans_out,
            );
            assert_float_eq!(actual_long_in, case.long_in, abs <= TOLERANCE, "{case:#?}");
            assert_float_eq!(
                actual_long_out,
                case.long_out,
                abs <= TOLERANCE,
                "{case:#?}"
            );
        }
    }
}
