use crate::scanner::Tk;

/*
 * But the identifier in a let statement doesn’t produce
 * a value, right? So why is it an Expression?
 *
 * ✅ It’s to keep things simple.
 */
pub struct IdentExpr;

impl IdentExpr {
    pub fn literal(name: &Tk) -> String {
        String::from(name)
    }

    pub(crate) fn display(
        f: &mut std::fmt::Formatter<'_>,
        name: &Tk,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "{}", String::from(name))?;
        Ok(())
    }
}
