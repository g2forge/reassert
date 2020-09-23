package com.g2forge.reassert.contract.v2.model.usagepropagation;

import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ETTerm<Term extends IUsageTerm, Edge extends IEdge, T> implements IETName<Term, Edge> {
	protected final Term term;
}
