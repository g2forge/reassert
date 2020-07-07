package com.g2forge.reassert.git;

import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.scanner.IScanner;
import com.g2forge.reassert.core.api.system.ISystem;

import lombok.AccessLevel;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@ToString
@EqualsAndHashCode(callSuper = false)
@RequiredArgsConstructor
@Getter(AccessLevel.PROTECTED)
public class GitSystem implements ISystem<GitCoordinates> {
	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	protected final IContext context;

	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	@Getter(lazy = true, value = AccessLevel.PUBLIC)
	private final GitRepository repository = new GitRepository(this);

	@Override
	public IDescriber<GitCoordinates> getCoordinateDescriber() {
		return GitCoordinatesDescriber.create();
	}

	@Override
	public IScanner getScanner() {
		return null;
	}

	@Override
	public GitCoordinates withSystem(GitCoordinates coordinates) {
		if (!isValid(coordinates)) throw new IllegalArgumentException();
		return coordinates.toBuilder().system(this).build();
	}
}
