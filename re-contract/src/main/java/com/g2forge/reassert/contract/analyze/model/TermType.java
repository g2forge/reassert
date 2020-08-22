package com.g2forge.reassert.contract.analyze.model;

import java.util.List;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.reassert.contract.analyze.model.TermType;
import com.g2forge.reassert.core.model.contract.ITerm;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum TermType {
	License(ILicenseTerm.class),
	Usage(IUsageTerm.class);

	public static TermType valueOf(ITerm term) {
		final List<TermType> types = HCollection.asSet(values()).stream().filter(t -> t.getType().isInstance(term)).collect(Collectors.toList());
		if (types.size() != 1) return null;
		return HCollection.getOne(types);
	}

	protected final Class<? extends ITerm> type;
}
