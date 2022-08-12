# Input validation of mapping parameters

    `canvas_network` must be a scalar with type <ip_network>.

---

    `canvas_network` must be a scalar with type <ip_network>.

---

    `pixel_prefix` must be a positive integer scalar.

---

    `pixel_prefix` must be a positive integer scalar.

---

    `pixel_prefix` must be a positive integer scalar.

---

    `curve` must be one of "hilbert" or "morton", not "hilber".
    i Did you mean "hilbert"?

---

    `curve` must be one of "hilbert" or "morton", not "hilber".
    i Did you mean "hilbert"?

---

    Pixel prefix length must not be greater than 32.
    i Canvas uses IPv4 address space.
    x Pixel has prefix length 33.

---

    Pixel prefix length must not be greater than 128.
    i Canvas uses IPv6 address space.
    x Pixel has prefix length 129.

---

    Pixel prefix length must be greater than canvas.
    x Canvas has prefix length 16.
    x Pixel has prefix length 14.

---

    The difference between canvas and pixel prefix lengths must be even.
    x Canvas has prefix length 0.
    x Pixel has prefix length 31.

---

    The difference between canvas and pixel prefix lengths must not be greater than 24.
    x Canvas has prefix length 0.
    x Pixel has prefix length 32.
    i These values would produce a plot with 65,536 x 65,536 pixels.

# Other input validation

    `address` must be a vector with type <ip_address>.

---

    `network` must be a vector with type <ip_network>.

