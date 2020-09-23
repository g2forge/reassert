package com.g2forge.reassert.contract.algorithm.usagepropagation.model.name;

import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class TermUsagePropagationName<Term extends IUsageTerm, Edge extends IEdge, T> implements IUsagePropagationName<Term, Edge> {
	protected final Term term;
}
