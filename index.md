What’s inside

01

### Product-name categorisation

The classifier ladder maps around 110 Swedish menopausal hormone therapy
product names onto the A1–I2 categories. Those categories cover local
and systemic estrogens, combined preparations, progestogens, implants,
intrauterine systems, tibolone and testosterone.

02

### Duration correction and gap bridging

`mht` corrects the dispensed durations of products whose recorded
defined daily dose is unreliable. It gives intrauterine systems their
real service life. It bridges gaps of up to four weeks between
consecutive prescriptions.

03

### Approach-based exposure definitions

`mht` writes the `rd_approach{1,2,3,3b}_{single,multiple}` variables
onto a person-week skeleton. It applies the `previous` and `exclude`
rules, and the three-year minimum-duration rule used by the
Skalkidou-lab MHT studies.
