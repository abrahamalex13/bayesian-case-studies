import pandas as pd
from information_theory.empirical_pmf import empirical_pmf


def test_pool_rare():

    x = pd.DataFrame({"Make": ["Honda", "Honda", "Tesla"]})

    epmf = empirical_pmf(
        x["Make"], nobs_lt_pool_other=2, categories=[], include_other=False
    )
    epmf["Make"] = epmf["Make"].astype("object")

    expected = pd.DataFrame({"Make": ["Honda", "OTHER"], "n": [2, 1]})

    pd.testing.assert_frame_equal(epmf[["Make", "n"]], expected)


def test_thresh_no_pool_rare():

    x = pd.DataFrame({"Make": ["Honda", "Honda", "Tesla", "Tesla"]})

    epmf = empirical_pmf(
        x["Make"], nobs_lt_pool_other=2, categories=[], include_other=False
    )
    epmf["Make"] = epmf["Make"].astype("object")

    expected = pd.DataFrame({"Make": ["Honda", "Tesla"], "n": [2, 2]})

    pd.testing.assert_frame_equal(epmf[["Make", "n"]], expected)


def test_thresh_no_pool_rare_incl_other():

    x = pd.DataFrame({"Make": ["Honda", "Honda", "Tesla", "Tesla"]})

    epmf = empirical_pmf(
        x["Make"], nobs_lt_pool_other=2, categories=[], include_other=True
    )
    epmf["Make"] = epmf["Make"].astype("object")

    expected = pd.DataFrame(
        {"Make": ["Honda", "Tesla", "OTHER"], "n": [2, 2, 0]}
    )

    pd.testing.assert_frame_equal(epmf[["Make", "n"]], expected)


def test_predefined_categories_drop_novel_obs():
    """
    Demonstration of expected, though often not-preferred, behavior.
    """

    x = pd.DataFrame({"Make": ["Honda", "Honda", "Tesla", "Tesla"]})

    epmf = empirical_pmf(
        x["Make"],
        nobs_lt_pool_other=2,
        categories=["Honda", "Ford"],
        include_other=False,
    )
    epmf["Make"] = epmf["Make"].astype("object")

    expected = pd.DataFrame({"Make": ["Honda", "Ford"], "n": [2, 0]})

    pd.testing.assert_frame_equal(epmf[["Make", "n"]], expected)


def test_predefined_categories_include_other_novel_obs():

    x = pd.DataFrame({"Make": ["Honda", "Honda", "Tesla", "Tesla"]})

    epmf = empirical_pmf(
        x["Make"],
        nobs_lt_pool_other=1,
        categories=["Honda", "Ford"],
        include_other=True,
    )
    epmf["Make"] = epmf["Make"].astype("object")

    expected = pd.DataFrame(
        {"Make": ["Honda", "OTHER", "Ford"], "n": [2, 2, 0]}
    )

    pd.testing.assert_frame_equal(epmf[["Make", "n"]], expected)
