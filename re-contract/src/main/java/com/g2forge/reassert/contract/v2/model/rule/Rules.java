package com.g2forge.reassert.contract.v2.model.rule;

import java.util.Collection;

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
}