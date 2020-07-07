package com.g2forge.reassert.core.api.scanner;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.reassert.core.api.IReassertGraphBuilder;
import com.g2forge.reassert.core.model.artifact.Artifact;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class CompositeScanner implements IScanner {
	protected final Collection<IScanner> scanners;

	public CompositeScanner(IScanner... scanners) {
		this(HCollection.asList(scanners));
	}

	@Override
	public boolean isRelevant(IScanner.IItem item) {
		return getScanners().stream().map(s -> s.isRelevant(item)).reduce(false, Boolean::logicalOr);
	}

	@Override
	public void load(Collection<IScanner.IItem> items, Artifact<?> container, IReassertGraphBuilder builder) {
		for (IScanner scanner : getScanners()) {
			final List<IScanner.IItem> relevant = items.stream().filter(scanner::isRelevant).collect(Collectors.toList());
			scanner.load(relevant, container, builder);
		}
	}

}
