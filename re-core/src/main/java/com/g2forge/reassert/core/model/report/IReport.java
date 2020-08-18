package com.g2forge.reassert.core.model.report;

import java.util.Collection;

import org.jgrapht.Graph;
import org.slf4j.event.Level;

import com.g2forge.alexandria.java.adt.compare.ComparableComparator;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;

public interface IReport {
	public Collection<IFinding> getFindings();

	public Graph<IVertex, IEdge> getGraph();

	public default Level getMinLevel() {
		final Collection<IFinding> findings = getFindings();
		if (findings.isEmpty()) return null;
		return findings.stream().map(IFinding::getLevel).min(ComparableComparator.create()).get();
	}

	public IOrigins getOrigins();

	public default boolean isAcceptable() {
		final Level minLevel = getMinLevel();
		if (minLevel == null) return true;
		return minLevel.compareTo(Level.WARN) >= 0;
	}
}
