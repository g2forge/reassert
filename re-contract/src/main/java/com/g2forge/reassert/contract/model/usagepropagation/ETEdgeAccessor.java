package com.g2forge.reassert.contract.model.usagepropagation;

import java.util.Objects;

import com.g2forge.alexandria.analysis.ISerializableFunction1;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ETEdgeAccessor<Term extends IUsageTerm, Edge extends IEdge, T> implements IETName<Term, Edge> {
	protected final ISerializableFunction1<? super Edge, ? extends T> accessor;

	protected final IFunction1<? super T, ? extends TermRelation> adapter;

	@Override
	public boolean equals(Object obj) {
		if (this == obj) return true;
		if (obj == null) return false;
		if (getClass() != obj.getClass()) return false;

		final ETEdgeAccessor<?, ?, ?> that = (ETEdgeAccessor<?, ?, ?>) obj;
		if (!getAccessor().asMethodAnalyzer().getPath().equals(that.getAccessor().asMethodAnalyzer().getPath())) return false;
		if (!getAdapter().equals(that.getAdapter())) return false;
		return true;
	}

	@Override
	public int hashCode() {
		return Objects.hash(getAccessor().asMethodAnalyzer().getPath(), getAdapter());
	}
}
