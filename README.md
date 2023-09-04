# [RFC 8288][rfc8288] parser

[RFC 8288][rfc8288] specifies the convention for parsing the [Link][link-header] header.

## Usage

```rust
use nom_rfc8288::complete::{link, LinkData, LinkParam};

let link_data = r#"<https://example.com>; rel="origin"; csv="one,two""#;
let parsed = link(link_data).unwrap();

assert_eq!(
    parsed,
    vec![
        Some(
            LinkData {
                url: "https://example.com",
                params: vec![
                    LinkParam {
                        key: "rel",
                        val: Some("origin".to_owned()),
                    },
                    LinkParam {
                        key: "csv",
                        val: Some("one,two".to_owned()),
                    }
                ],
            }
        ),
    ]
);
```

## Contributing

### Pre-commit hooks

See the `pre-commit` [quick start](https://pre-commit.com/#quick-start) guide for how to setup `pre-commit`.

[rfc8288]: https://datatracker.ietf.org/doc/html/rfc8288
[link-header]: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Link
