import pandas as pd


def empirical_pmf(x, nobs_lt_pool_other, categories=[], include_other=False):
    """
    Given discrete-valued data vector `x`, augment frequency tabulations. 
    
    Treat high cardinality: pool OTHER from rare categories 
    (nobs < nobs_lt_pool_other).

    May pre-define pmf categories, pooling novel categories into OTHER.
    (Ensures alignment between distributions in KL Divergence, for example.)
    
    For pmf exhaustiveness, may ensure OTHER category.

    """

    var = x.name

    freq = x.value_counts().reset_index(drop=False)
    freq.columns = [var, "n"]
    if (levels_rare := freq[var][freq["n"] < nobs_lt_pool_other].tolist()) :
        x[x.isin(levels_rare)] = "OTHER"

    if not categories:
        categories = list(x.unique())
    if include_other:
        if "OTHER" not in categories:
            categories += ["OTHER"]

    if (levels_novel := x[~x.isin(categories)].unique().tolist()) :
        x[x.isin(levels_novel)] = "OTHER"

    # pandas categorical-type tabs frequency over pre-specified categories
    x = pd.Series(pd.Categorical(x, categories=categories, ordered=False))

    freq = x.value_counts().reset_index(drop=False)
    freq.columns = [var, "n"]
    freq["prb"] = freq["n"] / freq["n"].sum()
    freq = freq.sort_values("prb", ascending=False)

    return freq
