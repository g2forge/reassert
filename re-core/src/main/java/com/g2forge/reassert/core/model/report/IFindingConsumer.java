package com.g2forge.reassert.core.model.report;

import java.util.Collection;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.reassert.core.model.IVertex;

public interface IFindingConsumer {
	public void found(final Collection<? extends IFinding> findings, final Collection<? extends IVertex> vertices);

	public default void found(final Collection<? extends IFinding> findings, final IVertex... vertices) {
		found(findings, HCollection.asList(vertices));
	}

	public IVertex found(final IFinding finding, final Collection<? extends IVertex> vertices);

	public default IVertex found(final IFinding finding, final IVertex... vertices) {
		return found(finding, HCollection.asList(vertices));
	}
}
