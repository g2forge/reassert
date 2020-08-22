package com.g2forge.reassert.list;

import com.g2forge.reassert.core.api.IReassertGraphBuilder;
import com.g2forge.reassert.core.api.system.ARepository;
import com.g2forge.reassert.core.model.artifact.Artifact;

import lombok.AccessLevel;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@ToString
@EqualsAndHashCode(callSuper = false)
@Getter(AccessLevel.PROTECTED)
@RequiredArgsConstructor
public abstract class AListRepository extends ARepository<ListCoordinates, ListSystem> {
	@Override
	public ListSystem getSystem() {
		return ListSystem.create();
	}

	protected abstract void internal(ListCoordinates coordinates, IReassertGraphBuilder builder);

	@Override
	public Artifact<ListCoordinates> load(ListCoordinates coordinates, IReassertGraphBuilder builder) {
		assertValid(coordinates);
		internal(coordinates, builder);
		return null;
	}
}
