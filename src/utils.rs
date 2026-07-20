pub fn oxford_join<I, T>(iter: I) -> String
where
    T: ToString,
    I: IntoIterator<Item = T>,
{
    let mut iter = iter.into_iter();
    let Some(next) = iter.next() else {
        return String::new();
    };

    let mut out = String::new();
    out.push_str(next.to_string().as_ref());

    // We have a second item!
    if let Some(mut buf) = iter.next() {
        let mut many = false;
        for next in iter.map(|n| core::mem::replace(&mut buf, n)) {
            // Add the _previous_ value to the output. The "current" value
            // is now in the buffer.
            out.push_str(", ");
            out.push_str(next.to_string().as_ref());
            many = true;
        }
        if many {
            out.push_str(", ");
        } else {
            out.push(' ');
        }
        out.push_str("or ");

        // Cap it off with the last item.
        out.push_str(buf.to_string().as_ref());
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_oxford_join() {
        assert_eq!(oxford_join(["foo"]), "foo");
        assert_eq!(oxford_join(["foo", "bar"]), "foo or bar");
        assert_eq!(oxford_join(["foo", "bar", "baz"]), "foo, bar, or baz");
    }
}
