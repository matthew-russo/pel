#[derive(Debug, Clone)]
pub(crate) struct FileLocation {
    pub(crate) line: u32,
    pub(crate) col: u32,
}

#[derive(Debug, Clone)]
pub(crate) struct LocationContext {
    pub(crate) file: String,
    pub(crate) start_location: FileLocation,
    pub(crate) end_location: FileLocation,
}

impl LocationContext {
    pub(crate) fn merge(lc1: &LocationContext, lc2: &LocationContext) -> LocationContext {
        assert_eq!(lc1.file, lc2.file);

        LocationContext {
            file: lc1.file.clone(),
            start_location: min_location(&lc1.start_location, &lc2.start_location),
            end_location: max_location(&lc1.end_location, &lc2.end_location)
        }
    }
}

fn min_location(fl1: &FileLocation, fl2: &FileLocation) -> FileLocation {
    if fl1.line < fl2.line {
        fl1.clone()
    } else if fl2.line > fl1.line {
        fl2.clone()
    } else {
        if fl1.col <= fl2.col {
            fl1.clone()
        } else {
            fl2.clone()
        }
    }
}

fn max_location(fl1: &FileLocation, fl2: &FileLocation) -> FileLocation {
    if fl1.line < fl2.line {
        fl2.clone()
    } else if fl2.line > fl1.line {
        fl1.clone()
    } else {
        if fl1.col < fl2.col {
            fl2.clone()
        } else {
            fl1.clone()
        }
    }
}

pub(crate) fn discriminants_equal<T>(expected: &T, actual: &T) -> bool {
    let expected_discriminant = std::mem::discriminant(expected);
    let actual_discriminant = std::mem::discriminant(actual);
    expected_discriminant == actual_discriminant
}
