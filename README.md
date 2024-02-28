# hetero-zip

A `Traversable`'s elements can be visited one at a time, and updated in-place.
That means we can visit them at the same time as we walk along a list, and use
the values in the list to update the values in the `Traversable`. This library
does just that.
