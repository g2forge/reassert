package com.g2forge.reassert.maven;

import com.g2forge.alexandria.annotations.service.Service;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.module.IModule;

@Service(IModule.class)
public class MavenModule implements IModule, ISingleton {
	protected static final MavenModule INSTANCE = new MavenModule();

	public static MavenModule create() {
		return INSTANCE;
	}

	private MavenModule() {}

	@Override
	public Loaded load(IContext context) {
		return IModule.Loaded.builder().system(new MavenSystem(context)).describer(MavenCoordinatesDescriber.create()).describer(new MavenPOMDescriber(context)).build();
	}
}
