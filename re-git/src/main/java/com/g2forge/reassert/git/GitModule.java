package com.g2forge.reassert.git;

import com.g2forge.alexandria.annotations.service.Service;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.module.IModule;

@Service(IModule.class)
public class GitModule implements IModule, ISingleton {
	protected static final GitModule INSTANCE = new GitModule();

	public static GitModule create() {
		return INSTANCE;
	}

	private GitModule() {}

	@Override
	public Loaded load(IContext context) {
		return IModule.Loaded.builder().system(new GitSystem(context)).describer(GitCoordinatesDescriber.create()).build();
	}
}
