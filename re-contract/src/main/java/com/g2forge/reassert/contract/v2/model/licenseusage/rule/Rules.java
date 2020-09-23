package com.g2forge.reassert.contract.v2.model.licenseusage.rule;

import java.util.Collection;

import com.g2forge.alexandria.java.core.helpers.HCollection;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class Rules implements IRules {
	@Singular
	protected final Collection<IRule> rules;

	public Rules(IRule... rules) {
		this(HCollection.asList(rules));
	}
}