package com.g2forge.reassert.core.model.contract.terms;

public enum TermRelation {
	Unspecified,
	Included,
	Excluded;

	public static TermRelation valueOf(boolean bool) {
		return TermRelation.valueOf(Boolean.valueOf(bool));
	}

	public static TermRelation valueOf(Boolean bool) {
		if (bool == null) return TermRelation.Unspecified;
		return bool ? TermRelation.Included : TermRelation.Excluded;
	}
}