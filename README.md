
## Disclaimer

This project was started by me when working on the Cardano Wallet. Due to
the fact the original repository has been [archived](https://github.com/input-output-hk/cardano-wallet-legacy) but I still think this
project is useful as a standalone library, I have performed surgery to extract
it out. Original copyright applies.

## Example

You can find a rehashing of the original [servant tutorial](http://docs.servant.dev/en/stable/tutorial/Server.html) in the `example`
folder. The only two notable differences for the user are:

1. The `API` type now includes a `FilterBy` and `SortBy` type families;
2. The user has to write a bunch of boilerpate instances (which could in
   principle be factored away via `generics-sop`) and pick a specific backend
   (at the moment only `ixset-typed` is offered).

### Demo

Once the server starts, the user has access to a number of useful sorting
& filter queries "for free" (modulo boilerpate). Furthermore, when using
something like `servant_swagger` now the filter & sort operation can fairly
easily be reflected into the documentation. An example of how this might look
like can be found in the older [Cardano Wallet API documentation](
https://cardanodocs.com/technical/wallet/api/v1/?v=1.4.0#tag/Wallets%2Fpaths%2F~1api~1v1~1wallets%2Fget).

#### Filtering by id (EQ)

Request: `http://localhost:8080/users?id=1`

![Peek 2019-09-29 12-19](https://user-images.githubusercontent.com/442035/65830834-93e82a00-e2b3-11e9-8cf4-c51a497b6113.gif)

#### Filtering by id (LT)

Request: `http://localhost:8080/users?id=LT[2]`

![Peek 2019-09-29 12-23](https://user-images.githubusercontent.com/442035/65830884-01945600-e2b4-11e9-95e1-6bb278f79967.gif)

#### Sorting by registration_date (ASC)

Request: `http://localhost:8080/users?sort_by=ASC[registration_date]`

![Peek 2019-09-29 12-24](https://user-images.githubusercontent.com/442035/65830948-5c2db200-e2b4-11e9-8e3a-52f13c179eba.gif)

#### Sorting by registration_date (DESC)

Request: `http://localhost:8080/users?sort_by=DES[registration_date]`

![Peek 2019-09-29 12-26](https://user-images.githubusercontent.com/442035/65830952-664fb080-e2b4-11e9-9769-d8c932879b1e.gif)

## Credits

Written mostly by Alfredo Di Napoli (me) during winter 2017. The ripped-out
modules have received contributions from (on top of my head):

- Edsko de Vries (for the `IxSet.Indices` part);
- Matt Parsons (general hacking on the type families, documentation, etc);
- The IOHK Engineering Team (misc).

If I have left out somebody, feel free to open a PR.
