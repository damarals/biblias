from books import BOOKS, by_id, by_code


def test_has_66_books_in_order():
    assert len(BOOKS) == 66
    assert [b.id for b in BOOKS] == list(range(1, 67))


def test_lookup_by_id_and_code():
    assert by_id(1).code == "GEN"
    assert by_code("REV").id == 66


def test_first_timothy_abbrev_is_fixed():
    # Regression: the R code had the typo "1Tn"; correct PT abbrev is "1Tm".
    assert by_code("1TI").abbrev == "1Tm"
    assert by_id(54).name == "1 Timóteo"


def test_testament_split():
    assert by_id(39).testament == "OT"
    assert by_id(40).testament == "NT"
